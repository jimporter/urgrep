;;; urgrep.el --- Universal recursive grep -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/urgrep
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((emacs "27"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A universal frontend to various grep-like tools. Currently, ripgrep, ag, ack,
;; git-grep, and grep are supported.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'grep)
(require 'project)
(require 'text-property-search)

(defgroup urgrep nil
  "Run a grep-like command and display the results."
  :group 'tools
  :group 'processes)

(defcustom urgrep-group-matches t
  "Group matches by the file they were found in."
  :type 'boolean
  :group 'urgrep)

(defcustom urgrep-search-regexp nil
  "Default to searching via regexp."
  :type 'boolean
  :group 'urgrep)

(defcustom urgrep-regexp-syntax 'bre
  "Default syntax to use for regexp searches."
  :type '(choice (const :tag "Basic regexp" bre)
                 (const :tag "Extended regexp" ere)
                 (const :tag "Perl-compatible regexp" pcre))
  :group 'urgrep)

(defcustom urgrep-case-fold 'inherit
  "Default case-sensitivity for searches.
Valid values are nil (case-sensitive), t (case-insensitive), `smart'
\(case-insensitive if the query is all lower case), and `inherit'
\(case-sensitive if `case-fold-search' is nil, \"smart\" otherwise)."
  :type '(choice (const :tag "Case sensitive" nil)
                 (const :tag "Smart case" 'smart)
                 (const :tag "Inherit from `case-fold-search'" 'inherit)
                 (const :tag "Case insensitive" t))
  :group 'urgrep)

(defcustom urgrep-context-lines 0
  "Number of lines of context to show.
If this is an integer, show that many lines of context on either side.
If a cons, show CAR and CDR lines before and after, respectively."
  :type '(choice integer (cons integer integer))
  :group 'urgrep)

(defface urgrep-hit '((t :inherit compilation-info))
  "Face for matching files."
  :group 'urgrep)

(defface urgrep-match-count '((t :inherit compilation-info))
  "Face for match counts."
  :group 'urgrep)

(defface urgrep-match '((t :inherit match))
  "Face for matching text."
  :group 'urgrep)

(defface urgrep-context '((t :inherit shadow))
  "Face for context lines."
  :group 'urgrep)


;; Urgrep tools

(defconst urgrep--context-arguments
  '(((or '(0 . 0) 0) nil)
    (`(,b . 0) (list (format "-B%d" b)))
    (`(0 . ,a) (list (format "-A%d" a)))
    ((or `(,c . ,c) (and c (pred numberp))) (list (format "-C%d" c)))
    (`(,b . ,a) (list (format "-B%d" b) (format "-A%d" a)))))

(cl-defun urgrep--rgrep-command (query &key tool regexp context
                                       &allow-other-keys)
  "Get the command to run for QUERY when using rgrep.
Optional keys TOOL, REGEXP, and CONTEXT are as in `urgrep-command'."
  (grep-compute-defaults)
  ;; Locally add options to `grep-find-template' that grep.el isn't aware of.
  (let ((grep-find-template grep-find-template))
    (dolist (i `((regexp-arguments  . ,regexp)
                 (context-arguments . ,context)))
      (when-let ((args (urgrep-get-property-pcase tool (car i) (cdr i)))
                 (args (mapconcat #'urgrep--maybe-shell-quote-argument args
                                  " "))
                 ((string-match "<C>" grep-find-template)))
        (setq grep-find-template
              (replace-match (concat args " <C>") t t grep-find-template))))
    (rgrep-default-command query "*" nil)))

(defun urgrep--rgrep-process-setup ()
  "Set up environment variables for rgrep.
See also `grep-process-setup'."
  ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
  ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'.
  (setenv "TERM" "emacs-urgrep")
  ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions.
  (setenv "GREP_COLOR" "01;31")
  ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions.
  (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne"))

(defvar urgrep-tools
  `(("ripgrep"
     (executable-name "rg")
     (regexp-syntax (pcre))
     (pre-arguments ("--color" "always" "--colors" "path:fg:magenta"
                     "--colors" "match:fg:red" "--colors" "match:style:bold"))
     (post-arguments ("--"))
     (group-arguments (('nil '("--no-heading"))
                       (_    '("--heading"))))
     (context-arguments ,urgrep--context-arguments)
     (regexp-arguments (('nil '("-F"))))
     (case-fold-arguments (((pred identity) '("-i")))))
    ("ag"
     (executable-name "ag")
     (regexp-syntax (pcre))
     (pre-arguments ("--color-path" "35" "--color-match" "1;31"))
     (post-arguments ("--"))
     (group-arguments (('nil '("--nogroup"))
                       (_    '("--group"))))
     (context-arguments ,urgrep--context-arguments)
     (regexp-arguments (('nil '("-Q"))))
     (case-fold-arguments (('nil '("-s"))
                           (_    '("-i")))))
    ("ack"
     (executable-name "ack")
     (regexp-syntax (pcre))
     (pre-arguments ("--color-filename" "magenta" "--color-match" "bold red"))
     (post-arguments ("--"))
     (group-arguments (('nil '("--nogroup"))
                       (_    '("--group"))))
     (context-arguments ,urgrep--context-arguments)
     (regexp-arguments (('nil '("-Q"))))
     (case-fold-arguments (((pred identity) '("-i")))))
    ("git-grep"
     (executable-name "git")
     (vc-backend "Git")
     (regexp-syntax (bre ere pcre))
     (pre-arguments ("--no-pager" "-c" "color.grep.filename=magenta"
                     "-c" "color.grep.match=bold red" "grep" "--color" "-n"
                     "--recurse-submodules"))
     (post-arguments ("-e"))
     (group-arguments (('t '("--heading" "--break"))))
     (context-arguments ,urgrep--context-arguments)
     (regexp-arguments (('bre  '("-G"))
                        ('ere  '("-E"))
                        ('pcre '("-P"))
                        (_     '("-F"))))
     (case-fold-arguments (((pred identity) '("-i")))))
    ("grep"
     (executable-name "grep")
     (regexp-syntax (bre ere pcre))
     (command-function ,#'urgrep--rgrep-command)
     (process-setup ,#'urgrep--rgrep-process-setup)
     (context-arguments ,urgrep--context-arguments)
     ;; XXX: On MS Windows, -P and -F seem to cause issues due to the default
     ;; locale. Setting LC_ALL=en_US.utf8 fixes this, but I'm not sure if this
     ;; is the right thing to do in general...
     (regexp-arguments (('bre  '("-G"))
                        ('ere  '("-E"))
                        ('pcre '("-P"))
                        (_     '("-F"))))))
  "An alist of known tools to try when running urgrep.")

(defcustom urgrep-preferred-tools nil
  "List of urgrep tools to search for.
This can be nil to use the default list of tools in `urgrep-tools'
or a list of tool names to try in descending order of preference."
  :type `(choice (const :tag "Default" nil)
                 (repeat :tag "List of tools"
                         (choice . ,(mapcar (lambda (i) (list 'const (car i)))
                                            urgrep-tools))))
  :group 'urgrep)

(defvar urgrep--host-defaults nil
  "Default urgrep values for each known host.
This is an alist of host symbols (`localhost' or a TRAMP host) and
the default tool to use on that host.")

(defun urgrep-get-property (tool prop)
  "Get the property PROP from TOOL, or nil if PROP is undefined."
  (when-let ((prop-entry (assoc prop (cdr tool))))
    (cadr prop-entry)))

(defun urgrep-get-property-pcase (tool prop value)
  "Get the property PROP from TOOL and use it as a `pcase' macro for VALUE."
  (when-let ((cases (urgrep-get-property tool prop))
             (block (append `(,#'pcase ',value) cases)))
    (eval block t)))

(defun urgrep--get-default-tool ()
  "Get the preferred urgrep tool from `urgrep-tools'.
This caches the default tool per-host in `urgrep--host-defaults'."
  (if-let ((host-id (intern (or (file-remote-p default-directory) "localhost")))
           (cached-tool-name (alist-get host-id urgrep--host-defaults)))
      (assoc cached-tool-name urgrep-tools)
    (let ((vc-backend-name)
          (saw-vc-tool-p nil))
      (cl-dolist (tool (or urgrep-preferred-tools urgrep-tools))
        (let* ((tool (if (stringp tool) (assoc tool urgrep-tools) tool))
               (tool-executable (urgrep-get-property tool 'executable-name))
               (tool-vc-backend (urgrep-get-property tool 'vc-backend)))
          (setq saw-vc-tool-p (or saw-vc-tool-p tool-vc-backend))
          ;; Cache the VC backend name if we need it.
          (when-let (((and tool-vc-backend (not vc-backend-name)))
                     (proj (project-current)))
            (setq vc-backend-name (vc-responsible-backend (project-root proj))))
          ;; If we find the executable (and it's for the right VC backend, if
          ;; relevant), cache it and then return it.
          (when (and (executable-find tool-executable t)
                     (or (not tool-vc-backend)
                         (string= vc-backend-name tool-vc-backend)))
            ;; So long as we didn't examine a VC-specific tool, we can cache
            ;; this result for future calls, since the result will always be the
            ;; same. If we *did* see a VC-specific tool, this host will use
            ;; different tools for different directories, so we can't cache
            ;; anything.
            (unless saw-vc-tool-p
              (add-to-list 'urgrep--host-defaults (cons host-id (car tool))))
            (cl-return tool)))))))

(defun urgrep-get-tool (&optional tool)
  "Get the urgrep tool for TOOL.
If TOOL is nil, get the default tool. If TOOL is a string, look it
up in `urgrep-tools'. Otherwise, return TOOL as-is."
  (pcase tool
    ('nil (urgrep--get-default-tool))
    ((and (pred stringp) tool) (assoc tool urgrep-tools))
    (tool tool)))

(defun urgrep--maybe-shell-quote-argument (argument)
  "Quote ARGUMENT if needed for passing to an inferior shell.
This works as `shell-quote-argument', but avoids quoting unnecessarily
for MS shells."
  (if (and (or (eq system-type 'ms-dos)
               (and (eq system-type 'windows-nt) (w32-shell-dos-semantics)))
           (not (string-match "[^-0-9a-zA-Z_./=]" argument)))
      argument
    (shell-quote-argument argument)))

(defun urgrep--get-best-syntax (syntax tool)
  "Return the regexp syntax closest to SYNTAX that TOOL supports."
  (let ((tool-syntaxes (urgrep-get-property tool 'regexp-syntax)))
    (cond ((not syntax) nil)
          ((memq syntax tool-syntaxes) syntax)
          ((and (eq syntax 'ere) (memq 'pcre tool-syntaxes)) 'pcre)
          ((and (eq syntax 'pcre) (memq 'extended tool-syntaxes)) 'ere)
          (t (car tool-syntaxes)))))

(defun urgrep--convert-regexp (expr from-syntax to-syntax)
  "Convert the regexp EXPR from FROM-SYNTAX to TO-SYNTAX."
  (cond ((and (not (eq from-syntax to-syntax))
              (or (eq from-syntax 'bre) (eq to-syntax 'bre)))
         ;; XXX: This is a bit of a hack, but xref.el contains an internal
         ;; function for converting between basic and extended regexps. It might
         ;; be wise to use our own implementation, but this should work for now.
         (require 'xref)
         (xref--regexp-to-extended expr))
        (t expr)))

(cl-defun urgrep-command (query &rest rest &key tool (group t) regexp
                                (case-fold 'inherit) (context 0))
  (let* ((regexp-syntax (if (eq regexp t) urgrep-regexp-syntax regexp))
         (tool (urgrep-get-tool tool))
         (tool-re-syntax (urgrep--get-best-syntax regexp-syntax tool))
         (query (urgrep--convert-regexp query regexp-syntax tool-re-syntax))
         (cmd-fun (urgrep-get-property tool 'command-function)))
    ;; Determine whether to search case-sensitively or not.
    (when (eq case-fold 'inherit)
      (setq case-fold (if case-fold-search 'smart nil)))
    (when (eq case-fold 'smart)
      (setq case-fold (isearch-no-upper-case-p query regexp-syntax)))
    ;; Build the command arguments.
    (if cmd-fun
        (apply cmd-fun query :tool tool :regexp regexp-syntax
               :case-fold case-fold rest)
      (let* ((executable (urgrep-get-property tool 'executable-name))
             (pre-args (urgrep-get-property tool 'pre-arguments))
             (arguments (urgrep-get-property tool 'post-arguments)))
        ;; Fill in various options according to the tool's argument syntax.
        (dolist (i `((regexp-arguments    . ,tool-re-syntax)
                     (case-fold-arguments . ,case-fold)
                     (context-arguments   . ,context)
                     (group-arguments     . ,group)))
          (when-let ((args (urgrep-get-property-pcase tool (car i) (cdr i))))
            (setq arguments (append args arguments))))
        ;; FIXME: Inside compile and dired buffers, `shell-quote-argument'
        ;; doesn't handle TRAMP right...
        (mapconcat #'urgrep--maybe-shell-quote-argument
                   (append `(,executable) pre-args arguments `(,query))
                   " ")))))


;; urgrep-mode

(defvar urgrep-num-matches-found 0
  "Running total of matches found. This will be set buffer-locally.")

;; Set the first column to 0 because that's how we currently count.
;; XXX: It might be worth changing this to 1 if we allow reading the column
;; number explicitly in the output.
(defvar urgrep-first-column 0)

(defvar-local urgrep-current-query nil
  "The most recent search query run in this buffer.")
(defvar-local urgrep-current-tool nil
  "The most recent search tool used in this buffer.")

(defun urgrep-search-again (&optional edit-command)
  "Re-run the previous search.
If EDIT-COMMAND is non-nil, the search can be edited."
  (interactive "P")
  (let* ((query (cond ((not edit-command) urgrep-current-query)
                      ((listp urgrep-current-query)
                       (apply #'urgrep--read-query urgrep-current-query))
                      (t (urgrep--read-command urgrep-current-query))))
         (command (if (listp query)
                      (apply #'urgrep-command query)
                    query)))
    (urgrep--start command query urgrep-current-tool)))

(defvar urgrep-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Don't inherit from `compilation-minor-mode-map',
    ;; because that introduces a menu bar item we don't want.
    (set-keymap-parent map special-mode-map)
    (define-key map [mouse-2] 'compile-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-o" 'compilation-display-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    (define-key map "g" 'urgrep-search-again)
    map)
  "Keymap for urgrep buffers.")

(easy-menu-define urgrep-menu-map urgrep-mode-map
  "Menu for urgrep buffers."
  '("Urgrep"
    ["Next Match" next-error
     :help "Visit the next match and corresponding location"]
    ["Previous Match" previous-error
     :help "Visit the previous match and corresponding location"]
    ["First Match" first-error
     :help "Restart at the first match, visit corresponding location"]
    "---"
    ["Repeat Search" recompile
     :help "Run search again"]
    ["Stop search" kill-compilation
     :help "Kill the currently running search process"]))

(defvar urgrep-mode-tool-bar-map
  ;; When bootstrapping, tool-bar-map is not properly initialized yet,
  ;; so don't do anything.
  (when (keymapp tool-bar-map)
    (let ((map (copy-keymap tool-bar-map)))
      (define-key map [undo] nil)
      (define-key map [separator-2] nil)
      (define-key-after map [separator-urgrep] menu-bar-separator)
      (tool-bar-local-item
       "left-arrow" 'previous-error-no-select 'previous-error-no-select map
       :rtl "right-arrow"
       :help "Goto previous match")
      (tool-bar-local-item
       "right-arrow" 'next-error-no-select 'next-error-no-select map
       :rtl "left-arrow"
       :help "Goto next match")
      (tool-bar-local-item
       "cancel" 'kill-compilation 'kill-compilation map
       :enable '(let ((buffer (compilation-find-buffer)))
                  (get-buffer-process buffer))
       :help "Stop search")
      (tool-bar-local-item
       "refresh" 'recompile 'recompile map
       :help "Restart search")
      map)))

(defconst urgrep-mode-line-matches
  `(" [" (:propertize (:eval (int-to-string urgrep-num-matches-found))
                      face urgrep-match-count
                      help-echo "Number of matches so far")
    "]"))

(defvar urgrep-mode-font-lock-keywords
  '(("^Urgrep started.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^Urgrep finished with \\(?:\\(\\(?:[0-9]+ \\)?match\\(?:es\\)? found\\)\\|\\(no matches found\\)\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'urgrep-match-count nil t)
     (2 'compilation-warning nil t))
    ("^Urgrep \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ;; Highlight context lines of various flavors.
    ("^\\(?:.+?\\([:-=\0]\\)\\)?[1-9][0-9]*\\([-=]\\).*\n"
     (0 'urgrep-context)
     (1 (if (eq (char-after (match-beginning 1)) ?\0)
            `(face nil display ,(match-string 2)))
        nil t))
    ;; Hide excessive part of rgrep command.
    ("^find \\(\\. -type d .*\\(?:\\\\)\\|\")\"\\)\\)"
     (1 (if grep-find-abbreviate grep-find-abbreviate-properties
          '(face nil abbreviated-command t))))))

(defun urgrep--column-begin ()
  "Look forwards for the match highlight to compute the beginning column."
  (let* ((beg (match-end 0))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (mbeg (text-property-any beg end 'font-lock-face 'urgrep-match)))
    (when mbeg
      (- mbeg beg))))

(defun urgrep--column-end ()
  "Look forwards for the match highlight to compute the ending column."
  (let* ((beg (match-end 0))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (mbeg (text-property-any beg end 'font-lock-face 'urgrep-match))
         (mend (and mbeg (next-single-property-change mbeg 'font-lock-face nil
                                                      end))))
    (when mend
      (- mend beg))))

(defun urgrep--grouped-filename ()
  "Look backwards for the filename when a match is found in grouped output."
  (save-excursion
    (if-let ((match (text-property-search-backward 'urgrep-file-name)))
        (buffer-substring (prop-match-beginning match)
                          (prop-match-end match)))))

(defconst urgrep-regexp-alist
  ;; XXX: Try to rely on ANSI escapes as with the match highlight?
  `(;; Ungrouped matches
    (,(concat
       "^\\(?:"
       ;; Parse using a null terminator after the filename when possible.
       "\\(?1:[^\0\n]+\\)\\(?3:\0\\)\\(?2:[0-9]+\\):"
       "\\|"
       ;; Fallback if we can't use null terminators after the filename.
       ;; Use [1-9][0-9]* rather than [0-9]+ to allow ":034:" in file names.
       "\\(?1:[^\n:]+?[^\n/:]\\):[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:"
       "\\)")
     1 2 (,#'urgrep--column-begin . ,#'urgrep--column-end)
     nil nil
     (3 '(face nil display ":")))

    ;; Grouped matches
    ("^\\([1-9][0-9]*\\):"
     ,#'urgrep--grouped-filename 1
     (,#'urgrep--column-begin . ,#'urgrep--column-end)))
  "Regexp used to match results.
See `compilation-error-regexp-alist' for format details.")

(defun urgrep-process-setup ()
  "Set up compilation variables for urgrep."
  (when-let ((tool-setup (urgrep-get-property urgrep-current-tool
                                              'process-setup)))
    (funcall tool-setup))
  (setq-local urgrep-num-matches-found 0
              compilation-exit-message-function 'urgrep-exit-message))

(defun urgrep-exit-message (status code msg)
  "Return a status message for urgrep results."
  (if (eq status 'exit)
      ;; This relies on the fact that `compilation-start'
      ;; sets buffer-modified to nil before running the command,
      ;; so the buffer is still unmodified if there is no output.
      (cond ((and (zerop code) (buffer-modified-p))
             (if (> urgrep-num-matches-found 0)
                 (cons (format (ngettext "finished with %d match found\n"
                                         "finished with %d matches found\n"
                                         urgrep-num-matches-found)
                               urgrep-num-matches-found)
                       "matched")
               '("finished with matches found\n" . "matched")))
            ((not (buffer-modified-p))
             '("finished with no matches found\n" . "no match"))
            (t
             (cons msg code)))
    (cons msg code)))

(defun urgrep-filter ()
  "Handle match highlighting escape sequences inserted by the process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight matches and delete ANSI escapes.
        (while (re-search-forward
                (concat "\\(?:"
                        "\033\\[0?1;31m"      ; Find the escapes together...
                        "\\|"
                        "\033\\[1m\033\\[31m" ; ... or apart.
                        "\\)\\(.*?\\)\033\\[0?m")
                end 1)
          (replace-match
           (propertize (match-string 1) 'face nil 'font-lock-face 'urgrep-match)
           t t)
          (cl-incf urgrep-num-matches-found))
        ;; Highlight matching filenames and delete ANSI escapes.
        (when urgrep-group-matches
          (goto-char beg)
          (while (re-search-forward "\033\\[35m\\(.*?\\)\033\\[0?m" end 1)
            (replace-match
             (propertize (match-string 1) 'face nil 'font-lock-face 'urgrep-hit
                         'urgrep-file-name t)
             t t)))

        ;; Delete all remaining escape sequences.
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(define-compilation-mode urgrep-mode "Urgrep"
  "A compilation mode for various grep-like tools."
  (setq-local tool-bar-map urgrep-mode-tool-bar-map
              compilation-process-setup-function 'urgrep-process-setup
              compilation-error-face 'urgrep-hit
              compilation-error-regexp-alist urgrep-regexp-alist
              compilation-mode-line-errors urgrep-mode-line-matches
              compilation-disable-input t
              compilation-error-screen-columns nil)
  (add-hook 'compilation-filter-hook 'urgrep-filter nil t))

(defmacro urgrep--with-killed-local-variable (variable &rest body)
  "Execute the forms in BODY with VARIABLE temporarily non-local."
  (declare (indent 1))
  `(if (local-variable-p ,variable)
       (with-temp-buffer ,@body)
     ,@body))

(defun urgrep--start (command query tool)
  "Start a urgrep process for COMMAND.
QUERY is the original argument list that generated COMMAND (or it may
be the same value as COMMAND). TOOL is the tool that was used to
generate the command. This sets `urgrep-current-query' and
`urgrep-current-tool' buffer-locally so that they can be used when
rerunning the search."
  (with-current-buffer
      ;; Dynamically bind `urgrep-current-tool' so that `urgrep-process-filter'
      ;; can consult it.
      (urgrep--with-killed-local-variable 'urgrep-current-tool
        (let ((urgrep-current-tool tool))
          (compilation-start command 'urgrep-mode)))
    (setq urgrep-current-query query
          urgrep-current-tool tool)
    (current-buffer)))


;; Minibuffer configuration

(defvar urgrep-search-history nil "History list for urgrep search queries.")
(defvar urgrep-command-history nil "History list for urgrep commands.")
(defvar-local urgrep--search-default nil
  "The default query for a urgrep search, used to update the prompt.")

(defun urgrep--search-default ()
  "Return the default thing to search for.
If the region is active, return that. Otherwise, return the symbol at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (when-let ((symbol (symbol-at-point)))
      (substring-no-properties (symbol-name symbol)))))

(defun urgrep--search-prompt (default)
  "Return the prompt to use when asking for the search query.
This depends on the current values of various urgrep options. DEFAULT indicates
the default query, if any."
  (concat "Search "
          (if urgrep-search-regexp "regexp" "string")
          (let ((block (append `(,#'pcase ',urgrep-context-lines)
                               urgrep--context-arguments)))
            (mapconcat (lambda (i) (concat " " i)) (eval block t) ""))
          (when default
            (format " (default %s)" default))
          ": "))

(defun urgrep--update-search-prompt ()
  "Update the search prompt in the minibuffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((inhibit-read-only t)
           (match (text-property-search-forward 'field t t))
           (begin (prop-match-beginning match))
           (end (prop-match-end match))
           (props (text-properties-at begin))
           (prompt (urgrep--search-prompt urgrep--search-default)))
      (delete-region begin end)
      (insert (apply #'propertize prompt props))))
  ;; Fix up the point if it ends up in the prompt; this can happen if the point
  ;; was at the beginning of the editable text.
  (if (< (point) (minibuffer-prompt-end)) (goto-char (minibuffer-prompt-end))))

(defun urgrep-toggle-regexp ()
  "Toggle whether or not to use regexps for the search query.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive)
  (setq urgrep-search-regexp (not urgrep-search-regexp))
  (when (window-minibuffer-p) (urgrep--update-search-prompt)))

(defun urgrep-toggle-case-fold ()
  "Toggle whether or not to search case-sensitively.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive)
  (message (if (setq urgrep-case-fold (if urgrep-case-fold nil 'always))
               "case insensitive"
             "case sensitive")))

(defun urgrep--read-context (prompt)
  "Read the number of lines of context, prompting with PROMPT.
If called with a prefix argument, use its numeric value instead of
prompting."
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    (let ((enable-recursive-minibuffers t))
      (read-number prompt))))

(defun urgrep-set-context (lines)
  "Set the number of LINES of context to show in the search results.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive (list (urgrep--read-context "Context: ")))
  (setq urgrep-context-lines lines)
  (when (window-minibuffer-p) (urgrep--update-search-prompt)))

(defun urgrep-set-before-context (before-lines)
  "Set the number of BEFORE-LINES of context to show in the search results.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive (list (urgrep--read-context "Context before: ")))
  (let ((after-lines (if (consp urgrep-context-lines)
                         (cdr urgrep-context-lines)
                       urgrep-context-lines)))
    (setq urgrep-context-lines (cons before-lines after-lines)))
  (when (window-minibuffer-p) (urgrep--update-search-prompt)))

(defun urgrep-set-after-context (after-lines)
  "Set the number of AFTER-LINES of context to show in the search results.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive (list (urgrep--read-context "Context after: ")))
  (let ((before-lines (if (consp urgrep-context-lines)
                          (car urgrep-context-lines)
                       urgrep-context-lines)))
    (setq urgrep-context-lines (cons before-lines after-lines)))
  (when (window-minibuffer-p) (urgrep--update-search-prompt)))

(defvar urgrep-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\M-sr" #'urgrep-toggle-regexp)
    (define-key map "\M-sc" #'urgrep-toggle-case-fold)
    (define-key map "\M-sC" #'urgrep-set-context)
    (define-key map "\M-sB" #'urgrep-set-before-context)
    (define-key map "\M-sA" #'urgrep-set-after-context)
    map))

(cl-defun urgrep--read-query (initial &key tool (group urgrep-group-matches)
                                      (regexp urgrep-search-regexp)
                                      (case-fold urgrep-case-fold)
                                      (context urgrep-context-lines))
  "Prompt the user for a search query starting with an INITIAL value.
Return a list that can be passed to `urgrep-command' to turn into a shell
command. TOOL, GROUP, REGEXP, CASE-FOLD, and CONTEXT are as in
`urgrep-command'."
  (let* ((urgrep-search-regexp regexp)
         (urgrep-case-fold case-fold)
         (urgrep-context-lines context)
         (default (and (not initial) (urgrep--search-default)))
         (prompt (urgrep--search-prompt default))
         (query (minibuffer-with-setup-hook
                    (lambda () (setq-local urgrep--search-default default))
                  (read-from-minibuffer prompt initial urgrep-minibuffer-map nil
                                        'urgrep-search-history default)))
         (query (if (equal query "") default query)))
    (list query :tool (urgrep-get-tool tool) :group group
          :regexp urgrep-search-regexp :case-fold urgrep-case-fold
          :context urgrep-context-lines)))

(defun urgrep--read-command (command)
  "Read a shell command to use for searching, with initial value COMMAND."
  (read-shell-command "Search command: " command
                      (if (equal (car urgrep-command-history) command)
                          '(urgrep-command-history . 1)
                        'urgrep-command-history)))


;; User-facing functions (and supporting helpers)

(defun urgrep--read-directory (arg)
  "Get the directory to search in.
If ARG is nil, return the project's root directory. If ARG's numeric
value is 4, return the current directory. Otherwise, prompt for the
directory."
  (cond
   ((not arg) (let ((proj (project-current)))
                (if proj (project-root proj) default-directory)))
   ((= (prefix-numeric-value arg) 4) default-directory)
   (t (read-directory-name "In directory: " nil nil t))))

;;;###autoload
(cl-defun urgrep (query directory &rest rest &allow-other-keys)
  "Recursively search in DIRECTORY for a given QUERY.

When called interactively, search in the project's root directory, or
the current directory if there is no current project.  With \\[universal-argument] prefix,
search in the current directory.  With two \\[universal-argument] prefixes, prompt for a
directory to search in.
\\<urgrep-minibuffer-map>
The following keys are bound in `urgrep-minibuffer-map', active when
entering the search query:

Type \\[urgrep-toggle-regexp] to toggle regular-expression mode.
Type \\[urgrep-toggle-case-fold] to toggle case-sensitive search.
Type \\[urgrep-set-context] to set the number of context lines.
  With a numeric prefix argument, set the context to that many
  lines.  Without a prefix, prompt for the number.
Type \\[urgrep-set-before-context] to set the number of before context lines.
Type \\[urgrep-set-after-context] to set the number of after context lines."
  (interactive
   (list (urgrep--read-query nil)
         (urgrep--read-directory current-prefix-arg)))
  (let* ((query (if (listp query) query (cons query rest)))
         (command (apply #'urgrep-command query))
         (tool (urgrep-get-tool (cadr (cl-member :tool query))))
         (default-directory (or directory default-directory)))
    (urgrep--start command query tool)))

;;;###autoload
(defun urgrep-run-command (command directory tool)
  "Recursively search in DIRECTORY using the given COMMAND.

When called interactively, this behaves like `urgrep', but allows you
to edit the command before running it."
  (interactive
   (let ((query (urgrep--read-query nil)))
     (list (urgrep--read-command (apply #'urgrep-command query))
           (urgrep--read-directory current-prefix-arg)
           (cadr (cl-member :tool query)))))
  (let ((tool (urgrep-get-tool tool))
        (default-directory (or directory default-directory)))
    (urgrep--start command command tool)))

(provide 'urgrep)

;;; urgrep.el ends here
