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

(defcustom urgrep-context-lines 0
  "Number of lines of context to show."
  :type 'integer
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

(cl-defun urgrep-rgrep--command (query &key &allow-other-keys)
  ;; XXX: Support literal/regexp and context settings.
  (grep-compute-defaults)
  (rgrep-default-command query "*" nil))

(defvar urgrep-tools
  `(("ripgrep"
     (executable-name "rg")
     (pre-arguments ("--color" "always" "--colors" "path:fg:magenta"
                     "--colors" "match:fg:red" "--colors" "match:style:bold"))
     (post-arguments ("--"))
     (group-arguments ((t   ("--heading"))
                       (nil ("--no-heading"))))
     (regexp-arguments ((nil ("-F"))))
     (context-arguments "-C%d"))
    ("ag"
     (executable-name "ag")
     (pre-arguments ("--color-path" "35" "--color-match" "1;31"))
     (post-arguments ("--"))
     (group-arguments ((t   ("--group"))
                       (nil ("--nogroup"))))
     (regexp-arguments ((nil ("-Q"))))
     (context-arguments "-C%d"))
    ("ack"
     (executable-name "ack")
     (pre-arguments ("--color-filename" "magenta" "--color-match" "bold red"))
     (post-arguments ("--"))
     (group-arguments ((t   ("--group"))
                       (nil ("--nogroup"))))
     (regexp-arguments ((nil ("-Q"))))
     (context-arguments "-C%d"))
    ("git-grep"
     (executable-name "git")
     (vc-backend "Git")
     (pre-arguments ("--no-pager" "-c" "color.grep.filename=magenta"
                     "-c" "color.grep.match=bold red" "grep" "--color" "-n"
                     "--recurse-submodules"))
     (post-arguments ("-e"))
     (group-arguments ((t ("--heading" "--break"))))
     (regexp-arguments ((nil ("-F"))))
     (context-arguments "-C%d"))
    ("grep"
     (executable-name "grep")
     (command-function ,#'urgrep-rgrep--command)))
  "An alist of known tools to try when running urgrep.")

(defun urgrep-get-property (tool prop)
  "Get a given property PROP from TOOL, or nil if PROP is undefined."
  (when-let ((prop-entry (assoc prop (cdr tool))))
    (cadr prop-entry)))

(defun urgrep-get-property-assoc (tool prop key)
  "Get a given property PROP from TOOL, selecting a KEY from the alist value."
  (when-let ((prop-value (urgrep-get-property tool prop))
             (assoc-value (assoc key prop-value)))
    (cadr assoc-value)))

(defun urgrep-get-tool ()
  "Get the preferred urgrep tool from `urgrep-tools'."
  (let ((vc-backend-name))
    (cl-dolist (tool urgrep-tools)
      (let ((tool-executable (urgrep-get-property tool 'executable-name))
            (tool-vc-backend (urgrep-get-property tool 'vc-backend)))
        ;; Cache the VC backend name if we need it.
        (when-let (((and tool-vc-backend (not vc-backend-name)))
                   (proj (project-current)))
          (setq vc-backend-name
                (vc-responsible-backend (project-root proj))))
        (when (and (executable-find tool-executable t)
                   (or (not tool-vc-backend)
                       (string= vc-backend-name tool-vc-backend)))
          (cl-return tool))))))

(defun urgrep--maybe-shell-quote-argument (argument)
  "Quote ARGUMENT if needed for passing to an inferior shell.
This works as `shell-quote-argument', but avoids quoting unnecessarily
for MS shells."
  (if (and (or (eq system-type 'ms-dos)
               (and (eq system-type 'windows-nt) (w32-shell-dos-semantics)))
           (not (string-match "[^-0-9a-zA-Z_./=]" argument)))
      argument
    (shell-quote-argument argument)))

(cl-defun urgrep-command (query &rest rest &key tool (group t) regexp
                                (context 0))
  (let* ((tool (or tool (urgrep-get-tool)))
         (cmd-fun (urgrep-get-property tool 'command-function)))
    (if cmd-fun
        (apply cmd-fun query rest)
      (let ((executable (urgrep-get-property tool 'executable-name))
            (pre-args (or (urgrep-get-property tool 'pre-arguments) '()))
            (arguments (or (urgrep-get-property tool 'post-arguments) '())))
        ;; Fill in group arguments. XXX: Maybe figure out a more flexible way to
        ;; do this?
        (when-let ((x (urgrep-get-property-assoc tool 'group-arguments group)))
          (setq arguments (append x arguments)))
        ;; Fill in regexp/literal arguments.
        (when-let ((x (urgrep-get-property-assoc tool 'regexp-arguments
                                                 regexp)))
          (setq arguments (append x arguments)))
        ;; Fill in context arguments.
        (when-let (((> context 0))
                   (prop (urgrep-get-property tool 'context-arguments))
                   (context-arg (format prop context)))
          (setq arguments (append (list context-arg) arguments)))
        ;; FIXME: Inside compile and dired buffers, `shell-quote-argument'
        ;; doesn't handle TRAMP right...
        (mapconcat #'urgrep--maybe-shell-quote-argument
                   (append `(,executable) pre-args arguments `(,query))
                   " ")))))


;; urgrep-mode

(defvar urgrep-search-history nil "History list for urgrep.")
(defvar urgrep-num-matches-found 0
  "Running total of matches found. This will be set buffer-locally.")

;; Set the first column to 0 because that's how we currently count.
;; XXX: It might be worth changing this to 1 if we allow reading the column
;; number explicitly in the output.
(defvar urgrep-first-column 0)

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
    (define-key map "g" 'recompile)
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
    (let ((match (text-property-search-backward 'urgrep-file-name)))
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
  ;; XXX: Abstract this grep-specific code out so other tools can do stuff like
  ;; this.
  ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
  ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'.
  (setenv "TERM" "emacs-urgrep")
  ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions.
  (setenv "GREP_COLOR" "01;31")
  ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions.
  (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:sl=:cx=:ne")
  (setq-local urgrep-num-matches-found 0
              compilation-exit-message-function 'urgrep-exit-message))

(defun urgrep-exit-message (status code msg)
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


;; Minibuffer configuration

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
          (when (> urgrep-context-lines 0)
            (format " -C%d" urgrep-context-lines))
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

(defun urgrep-set-context (lines)
  "Set the number of LINES of context to show in the search results.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive
   (list
    (if current-prefix-arg
        (prefix-numeric-value current-prefix-arg)
      (let ((enable-recursive-minibuffers t))
        (read-number "Context: ")))))
  (setq urgrep-context-lines lines)
  (when (window-minibuffer-p) (urgrep--update-search-prompt)))

(defvar urgrep-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-c\C-r" #'urgrep-toggle-regexp)
    (define-key map "\C-c\C-c" #'urgrep-set-context)
    map))

(cl-defun urgrep--read-query (&key (regexp urgrep-search-regexp)
                                   (context urgrep-context-lines))
  "Prompt the user for a search query.
Return a list that can be passed to `urgrep-command' to turn into a shell
command."
  (let* ((urgrep-search-regexp regexp)
         (urgrep-context-lines context)
         (default (urgrep--search-default))
         (prompt (urgrep--search-prompt default))
         (query (minibuffer-with-setup-hook
                    (lambda () (setq-local urgrep--search-default default))
                  (read-from-minibuffer prompt nil urgrep-minibuffer-map nil
                                        'urgrep-search-history default)))
         (query (if (equal query "") default query)))
    (list query :group urgrep-group-matches :regexp urgrep-search-regexp
          :context urgrep-context-lines)))


;; User-facing functions (and supporting helpers)

(defun urgrep--read-directory (arg)
  (cond
   ((not arg) (let ((proj (project-current)))
                (if proj (project-root proj) default-directory)))
   ((= (prefix-numeric-value arg) 4) default-directory)
   (t (read-directory-name "In directory: " nil nil t))))

;;;###autoload
(cl-defun urgrep (query directory &rest rest &key command &allow-other-keys)
  "Recursively search in DIRECTORY for a given QUERY.

When called interactively, search in the project's root directory, or
the current directory if there is no current project.  With \\[universal-argument] prefix,
search in the current directory.  With two \\[universal-argument] prefixes, prompt for a
directory to search in.
\\<urgrep-minibuffer-map>
The following keys are bound in `urgrep-minibuffer-map', active when
entering the search query:

Type \\[urgrep-toggle-regexp] to toggle regular-expression mode.
Type \\[urgrep-set-context] to set the number of context lines.
  With a numeric prefix argument, set the context to that many
  lines.  Without a prefix, prompt for the number."
  (interactive
   (list
    ;; Wrap the command in a list so that we can tell it's a real command, not
    ;; just a query. This gets around some limitations with mixing optional and
    ;; keyword arguments.
    (list (apply #'urgrep-command (urgrep--read-query)))
    (urgrep--read-directory current-prefix-arg)))
  (setq query (cond
               (command query)
               ((listp query) (car query))
               (t (apply #'urgrep-command query rest))))
  (let ((default-directory (or directory default-directory)))
    (compilation-start query 'urgrep-mode)))

(provide 'urgrep)

;;; urgrep.el ends here
