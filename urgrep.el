;;; urgrep.el --- Universal recursive grep -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/urgrep
;; Version: 0.1-dev
;; Keywords:
;; Package-Requires: ((emacs "27.1") (project "0.2.0"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A universal frontend to various grep-like tools.  Currently, ugrep, ripgrep,
;; ag, ack, git-grep, and grep are supported.  The primary entry point to this
;; package is the interactive function `urgrep' (which see).

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'esh-opt)
(require 'generator)
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
  :type '(radio (const :tag "Basic regexp" bre)
                (const :tag "Extended regexp" ere)
                (const :tag "Perl-compatible regexp" pcre))
  :group 'urgrep)

(defcustom urgrep-case-fold 'inherit
  "Default case-sensitivity for searches.
Valid values are nil (case-sensitive), t (case-insensitive), `smart'
\(case-insensitive if the query is all lower case), and `inherit'
\(case-sensitive if `case-fold-search' is nil, \"smart\" otherwise)."
  :type '(radio (const :tag "Case sensitive" nil)
                (const :tag "Smart case" smart)
                (const :tag "Inherit from `case-fold-search'" inherit)
                (const :tag "Case insensitive" t))
  :group 'urgrep)

(defcustom urgrep-context-lines 0
  "Number of lines of context to show.
If this is an integer, show that many lines of context on either
side.  If a cons, show CAR and CDR lines before and after,
respectively."
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

(defcustom urgrep-setup-hook nil
  "List of hook functions run by `urgrep-process-setup' (see `run-hooks').
The currently-used tool can be inspected from the hook via
`urgrep-current-tool'."
  :type 'hook
  :group 'grep)


;; Urgrep utility functions

;; `format-prompt' was added in Emacs 28.1.
(defalias 'urgrep--format-prompt
  (if (fboundp 'format-prompt)
      #'format-prompt
    (lambda (prompt default &rest format-args)
      (concat
       (apply #'format prompt format-args)
       (when default
         (format " (default %s)" default))
       ": "))))

;; `project-root' was added in project.el 0.3.0 (Emacs 28.1).
(defalias 'urgrep--project-root
  (if (fboundp 'project-root)
      #'project-root
    (lambda (project) (car (project-roots project)))))

(defun urgrep--convert-regexp (expr from-syntax to-syntax)
  "Convert the regexp EXPR from FROM-SYNTAX to TO-SYNTAX."
  (cond ((and (not (eq from-syntax to-syntax))
              (or (eq from-syntax 'bre) (eq to-syntax 'bre)))
         ;; XXX: This is a bit of a hack, but xref.el contains an internal
         ;; function for converting between basic and extended regexps.  It
         ;; might be wise to use our own implementation, but this should work
         ;; for now.
         (require 'xref)
         (xref--regexp-to-extended expr))
        (t expr)))

(defun urgrep--common-prefix (string1 string2)
  "Get the common prefix shared by STRING1 and STRING2."
  (let ((cmp (compare-strings string1 nil nil string2 nil nil)))
    (if (eq cmp t)
        string1
      (substring string1 0 (1- (abs cmp))))))

(defun urgrep--wildcard-to-regexp-hunk (wildcard syntax)
  "Convert WILDCARD to a SYNTAX style regexp.
Unlike `wildcard-to-regexp', this excludes the begin/end specifiers,
and escapes null characters."
  (if wildcard
      (let ((hunk (substring (wildcard-to-regexp wildcard) 2 -2)))
        (urgrep--convert-regexp (replace-regexp-in-string "\0" "\\\\000" hunk)
                                'bre syntax))
    ""))

(defun urgrep--wildcards-to-regexp (wildcards syntax)
  "Convert a list of WILDCARDS to a SYNTAX style regexp."
  (let ((to-re (lambda (i) (urgrep--wildcard-to-regexp-hunk i syntax)))
        (wildcards (cl-remove-duplicates wildcards :test #'string=)))
    (if (<= (length wildcards) 1)
        (concat "^" (funcall to-re (car wildcards)) "$")
      (let* ((prefix (cl-reduce #'urgrep--common-prefix wildcards))
             ;; Make sure our prefix doesn't contain an incomplete character
             ;; class.
             (prefix (car (split-string prefix "\\[")))
             (start (length prefix))
             (suffixes (if (eq start 0)
                           wildcards
                         (mapcar (lambda (i) (substring i start)) wildcards)))
             (esc (if (eq syntax 'bre) "\\" nil)))
        (concat "^" (funcall to-re prefix) esc "("
                (mapconcat to-re suffixes (concat esc "|")) esc ")$")))))

(defmacro urgrep--with-killed-local-variable (variable &rest body)
  "Execute the forms in BODY with VARIABLE temporarily non-local."
  (declare (indent 1))
  `(if (local-variable-p ,variable)
       (with-temp-buffer ,@body)
     ,@body))


;; Urgrep tools

(defconst urgrep--context-arguments
  '(((or '(0 . 0) 0) nil)
    (`(,b . 0) (list (format "-B%d" b)))
    (`(0 . ,a) (list (format "-A%d" a)))
    ((or `(,c . ,c) (and c (pred numberp))) (list (format "-C%d" c)))
    (`(,b . ,a) (list (format "-B%d" b) (format "-A%d" a)))))

(cl-defun urgrep--rgrep-command (query &key tool regexp case-fold files context
                                       color &allow-other-keys)
  "Get the command to run for QUERY when using rgrep.
Optional keys TOOL, REGEXP, CASE-FOLD, FILES, CONTEXT, and COLOR are
as in `urgrep-command'."
  (grep-compute-defaults)
  ;; Locally add options to `grep-find-template' that grep.el isn't aware of.
  (let ((grep-find-template grep-find-template)
        (grep-highlight-matches (if color 'always nil))
        (files (if files (mapconcat #'identity files " ") "*")))
    (pcase-dolist (`(,k . ,v) `((regexp-arguments    . ,regexp)
                                (case-fold-arguments . ,case-fold)
                                (context-arguments   . ,context)))
      (when-let ((args (urgrep--get-prop-pcase k tool v))
                 (args (mapconcat #'urgrep--maybe-shell-quote-argument args
                                  " "))
                 ((string-match "<C>" grep-find-template)))
        (setq grep-find-template
              (replace-match (concat "<C> " args) t t grep-find-template))))
    (let ((case-fold-search nil))
      (rgrep-default-command query files nil))))

(defun urgrep--rgrep-process-setup ()
  "Set up environment variables for rgrep.
See also `grep-process-setup'."
  ;; `setenv' modifies `process-environment' let-bound in `compilation-start'
  ;; Any TERM except "dumb" allows GNU grep to use `--color=auto'.
  (setenv "TERM" "emacs-urgrep")
  ;; GREP_COLOR is used in GNU grep 2.5.1, but deprecated in later versions.
  (setenv "GREP_COLOR" "01;31")
  ;; GREP_COLORS is used in GNU grep 2.5.2 and later versions.
  (setenv "GREP_COLORS" "mt=01;31:fn=35:ln=:bn=:se=:sl=:cx=:ne"))

(defvar urgrep-tools
  `((ugrep
     (executable-name . "ugrep")
     (regexp-syntax bre ere pcre)
     (arguments executable color "-n" "--ignore-files" file-wildcards group
                context case-fold regexp "-e" query)
     (regexp-arguments ('bre  '("-G"))
                       ('ere  '("-E"))
                       ('pcre '("-P"))
                       (_     '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (file-wildcards-arguments
      ((and x (pred identity))
       (mapcar (lambda (i) (concat "--include=" i)) x)))
     (group-arguments ((pred identity) '("--heading" "--break")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments
      ('nil '("--color=never"))
      (_    '("--color=always"
              "--colors=mt=01;31:fn=35:ln=:bn=:se=:sl=:cx=:ne"))))
    (ripgrep
     (executable-name . "rg")
     (regexp-syntax pcre)
     (arguments executable color file-wildcards group context case-fold regexp
                "--" query)
     (regexp-arguments ('nil '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (file-wildcards-arguments
      ((and x (pred identity))
       (flatten-list (mapcar (lambda (i) (cons "-g" i)) x))))
     (group-arguments ('nil '("--no-heading"))
                      (_    '("--heading")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments ('nil '("--color" "never"))
                      (_    '("--color" "always" "--colors" "path:fg:magenta"
                              "--colors" "match:fg:red" "--colors"
                              "match:style:bold"))))
    (ag
     (executable-name . "ag")
     (regexp-syntax pcre)
     (arguments executable color file-wildcards group context case-fold regexp
                "--" query)
     (regexp-arguments ('nil '("-Q")))
     (case-fold-arguments ('nil '("-s"))
                          (_    '("-i")))
     (file-wildcards-arguments
      ((and x (pred identity))
       (list "-G" (urgrep--wildcards-to-regexp x 'pcre))))
     (group-arguments ('nil '("--nogroup"))
                      (_    '("--group")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments ('nil '("--nocolor"))
                      (_    '("--color-path" "35" "--color-match" "1;31"))))
    (ack
     (executable-name . "ack")
     (regexp-syntax pcre)
     (arguments executable color file-wildcards group context case-fold regexp
                "--" query)
     (regexp-arguments ('nil '("-Q")))
     (case-fold-arguments ((pred identity) '("-i")))
     (file-wildcards-arguments
      ((and x (pred identity))
       (list "-G" (urgrep--wildcards-to-regexp x 'pcre))))
     (group-arguments ('nil '("--nogroup"))
                      (_    '("--group")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments ('nil '("--nocolor"))
                      (_    '("--color-filename" "magenta" "--color-match"
                              "bold red"))))
    (git-grep
     (executable-name . "git")
     ;; XXX: Since we use --no-index, maybe it would make sense to allow using
     ;; git grep even outside of git repos.  However, that doesn't play nicely
     ;; with people who want to customize the arguments.
     (vc-backend . "Git")
     (regexp-syntax bre ere pcre)
     (arguments executable "--no-pager" color "--no-index" "--exclude-standard"
                "-n" group context case-fold regexp "-e" query "--"
                file-wildcards)
     (regexp-arguments ('bre  '("-G"))
                       ('ere  '("-E"))
                       ('pcre '("-P"))
                       (_     '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (file-wildcards-arguments (x x))
     (group-arguments ((pred identity) '("--heading" "--break")))
     (context-arguments . ,urgrep--context-arguments)
     ;; git is a bit odd in that color specification happens *before* the
     ;; subcommand and turning colors on/off happens *after*, so
     ;; `color-arguments' needs to include the subcommand "grep".
     (color-arguments ('nil '("grep" "--no-color"))
                      (_    '("-c" "color.grep.filename=magenta" "-c"
                              "color.grep.match=bold red" "grep" "--color"))))
    (grep
     (executable-name . "grep")
     (regexp-syntax bre ere pcre)
     (command-function . ,#'urgrep--rgrep-command)
     (process-setup . ,#'urgrep--rgrep-process-setup)
     ;; XXX: On MS Windows, -P and -F seem to cause issues due to the default
     ;; locale.  Setting LC_ALL=en_US.utf8 fixes this, but I'm not sure if this
     ;; is the right thing to do in general...
     (regexp-arguments ('bre  '("-G"))
                       ('ere  '("-E"))
                       ('pcre '("-P"))
                       (_     '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (context-arguments . ,urgrep--context-arguments)))
  "An alist of known tools to try when running urgrep.")

(defcustom urgrep-preferred-tools nil
  "List of urgrep tools to search for.
This can be nil to use the default list of tools in `urgrep-tools'
or a list of tools to try in descending order of preference.  Each tool
can be either a symbol naming the tool or a cons cell of the tool name
and the path of the executable."
  :type `(choice
          (const :tag "Default" nil)
          (repeat :tag "List of tools"
                  ,(let* ((tool-names (mapcar (lambda (i) `(const ,(car i)))
                                              urgrep-tools))
                          (tool-choice `(choice :tag "Tool" . ,tool-names)))
                     (append tool-choice
                             `((cons :tag "(tool . path)"
                                     ,tool-choice (string :tag "Path")))))))
    :group 'urgrep)

(defvar urgrep--host-defaults nil
  "Default urgrep values for each known host.
This is an alist of host symbols (`localhost' or a Tramp host) and
the default tool to use on that host.")

(defun urgrep--get-prop (prop tool)
  "Get the property PROP from TOOL, or nil if PROP is undefined."
  (alist-get prop (cdr tool)))

(defun urgrep--get-prop-pcase (prop tool value)
  "Get the property PROP from TOOL and use it as a `pcase' macro for VALUE."
  (when-let ((cases (urgrep--get-prop prop tool))
             (block (append `(,#'pcase ',value) cases)))
    (eval block t)))

(iter-defun urgrep--iter-tools ()
  "Iterate over all the grep-like tools.
If `urgrep-preferred-tools' is non-nil, iterate over them, yielding
each tool, possibly modified with the executable path defined in
`urgrep-preferred-tools.'  Otherwise, iterate over `urgrep-tools'."
  (if urgrep-preferred-tools
      (dolist (pref urgrep-preferred-tools)
        (pcase-let* ((`(,name . ,path) (if (consp pref) pref (cons pref nil)))
                     (tool (assq name urgrep-tools))
                     (tool (if path
                               `(,(car tool) .
                                 ((executable-name . ,path) . ,(cdr tool)))
                             tool)))
          (iter-yield tool)))
    (dolist (tool urgrep-tools)
      (iter-yield tool))))

(defun urgrep--get-default-tool ()
  "Get the preferred urgrep tool from `urgrep-tools'.
This caches the default tool per-host in `urgrep--host-defaults'."
  (if-let ((host-id (intern (or (file-remote-p default-directory) "localhost")))
           (cached-tool (alist-get host-id urgrep--host-defaults)))
      cached-tool
    (let ((vc-backend-name)
          (saw-vc-tool-p nil))
      (cl-loop for tool iter-by (urgrep--iter-tools) do
               (let ((tool-executable (urgrep--get-prop 'executable-name tool))
                     (tool-vc-backend (urgrep--get-prop 'vc-backend tool)))
                 (setq saw-vc-tool-p (or saw-vc-tool-p tool-vc-backend))
                 ;; Cache the VC backend name if we need it.
                 (when-let (((and tool-vc-backend (not vc-backend-name)))
                            (proj (project-current)))
                   (setq vc-backend-name (vc-responsible-backend
                                          (urgrep--project-root proj))))
                 ;; If we find the executable (and it's for the right VC
                 ;; backend, if relevant), cache it and then return it.
                 (when (and (executable-find tool-executable t)
                            (or (not tool-vc-backend)
                                (string= vc-backend-name tool-vc-backend)))
                   ;; So long as we didn't examine a VC-specific tool, we can
                   ;; cache this result for future calls, since the result will
                   ;; always be the same.  If we *did* see a VC-specific tool,
                   ;; this host will use different tools for different
                   ;; directories, so we can't cache anything.
                   (unless saw-vc-tool-p
                     (add-to-list 'urgrep--host-defaults
                                  (cons host-id tool)))
                   (cl-return tool)))))))

(defun urgrep-get-tool (&optional tool)
  "Get the urgrep tool for TOOL.
If TOOL is nil, get the default tool.  If TOOL is a string, look it up
in `urgrep-tools'.  Otherwise, return TOOL as-is."
  (pcase tool
    ('nil (urgrep--get-default-tool))
    ((and (pred symbolp) tool) (assq tool urgrep-tools))
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
  (let ((tool-syntaxes (urgrep--get-prop 'regexp-syntax tool)))
    (cond ((not syntax) nil)
          ((memq syntax tool-syntaxes) syntax)
          ((and (eq syntax 'ere) (memq 'pcre tool-syntaxes)) 'pcre)
          ((and (eq syntax 'pcre) (memq 'extended tool-syntaxes)) 'ere)
          (t (car tool-syntaxes)))))

;;;###autoload
(cl-defun urgrep-command (query &key tool regexp (case-fold 'inherit) files
                                (group t) (context 0) (color t)
                                (directory default-directory))
  "Return a command to use to search for QUERY.
Several keyword arguments can be supplied to adjust the resulting
command:

TOOL: a tool from `urgrep-tools': a key-value pair from the list, just
the key, or nil to use the default tool.

REGEXP: the style of regexp to use for results; one of nil (fixed
strings), `bre' (basic regexp), `ere' (extend regexp), `pcre'
\(Perl-compatible regexp), or t (the default regexp style stored in
`urgrep-regexp-syntax').

CASE-FOLD: determine whether QUERY is case-sensitive or not; possible
values are as `urgrep-case-fold', defaulting to `inherit'.

FILES: a wildcard (or list of wildcards) to limit the files searched.

GROUP: show results grouped by filename (t, the default), or if nil,
prefix the filename on each result line.

CONTEXT: the number of lines of context to show around results; either
an integer (to show the same number of lines before and after) or a
cons (to show CAR and CDR lines before and after, respectively).

COLOR: non-nil (the default) if the output should use color.

DIRECTORY: the directory to search in, or nil to use the
`default-directory'."
  (let ((default-directory (or directory default-directory)))
    (with-connection-local-variables
     (let* ((regexp-syntax (if (eq regexp t) urgrep-regexp-syntax regexp))
            (files (if (listp files) files (list files)))
            (tool (urgrep-get-tool tool))
            (tool-re-syntax (urgrep--get-best-syntax regexp-syntax tool))
            (query (urgrep--convert-regexp query regexp-syntax tool-re-syntax))
            (cmd-fun (urgrep--get-prop 'command-function tool)))
       ;; Determine whether to search case-sensitively or not.
       (when (eq case-fold 'inherit)
         (setq case-fold (if case-fold-search 'smart nil)))
       (when (eq case-fold 'smart)
         (setq case-fold (isearch-no-upper-case-p query regexp-syntax)))
       ;; Build the command arguments.
       (if cmd-fun
           (funcall cmd-fun query :tool tool :regexp regexp-syntax
                    :case-fold case-fold :files files :group group
                    :context context :color color)
         (let* ((executable (urgrep--get-prop 'executable-name tool))
                (arguments (urgrep--get-prop 'arguments tool)))
           (setq arguments (cl-substitute executable 'executable arguments))
           (setq arguments (cl-substitute query 'query arguments))
           ;; Fill in various options according to the tool's argument syntax.
           (pcase-dolist (`(,k . ,v) `((regexp         . ,tool-re-syntax)
                                       (case-fold      . ,case-fold)
                                       (file-wildcards . ,files)
                                       (group          . ,group)
                                       (context        . ,context)
                                       (color          . ,color)))
             (let* ((prop (intern (concat (symbol-name k) "-arguments")))
                    (args (urgrep--get-prop-pcase prop tool v)))
               (setq arguments (cl-substitute args k arguments))))
           (setq arguments (flatten-list arguments))
           (mapconcat #'urgrep--maybe-shell-quote-argument arguments " ")))))))


;; urgrep-mode

(defvar urgrep-file-wildcards nil
  "Zero or more wildcards to limit the files searched.")
(defvar urgrep-num-matches-found 0
  "Running total of matches found.  This will be set buffer-locally.")
(defvar-local urgrep-current-query nil
  "The most recent search query run in this buffer.")
(defvar-local urgrep-current-tool nil
  "The most recent search tool used in this buffer.")

;; Set the first column to 0 because that's how we currently count.
;; XXX: It might be worth changing this to 1 if we allow reading the column
;; number explicitly in the output.
(defvar urgrep-first-column 0)

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
    (define-key map [mouse-2] #'compile-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-c\C-c" #'compile-goto-error)
    (define-key map "\C-m" #'compile-goto-error)
    (define-key map "\C-o" #'compilation-display-error)
    (define-key map "\C-c\C-k" #'kill-compilation)
    (define-key map "\M-n" #'compilation-next-error)
    (define-key map "\M-p" #'compilation-previous-error)
    (define-key map "\M-{" #'compilation-previous-file)
    (define-key map "\M-}" #'compilation-next-file)
    (define-key map "n" #'next-error-no-select)
    (define-key map "p" #'previous-error-no-select)
    (define-key map "{" #'compilation-previous-file)
    (define-key map "}" #'compilation-next-file)
    (define-key map "\t" #'compilation-next-error)
    (define-key map [backtab] #'compilation-previous-error)
    (define-key map "g" #'urgrep-search-again)
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

(defun urgrep-mode--looking-at-context-line ()
  "Return t if looking at a grep-like context line.

If so, this function sets the match data, with the first match group
indicating the separator between the line number and the match, and
the second group indicating the separator between the file name and
line number."
  ;; Use the `urgrep-file-name' property set by `urgrep-filter' to reliably
  ;; detect if the result was printed in grouped or ungrouped format.
  (if (get-text-property (point) 'urgrep-file-name)
      ;; Ungrouped result.
      (let* ((file-name-end (next-single-property-change
                             (point) 'urgrep-file-name))
             (file-name (buffer-substring-no-properties (point) file-name-end)))
        (looking-at (concat
                     (regexp-quote file-name)
                     "\\(?:\\(?2:\0\\)\\|[:=-]\\)"
                     "[0-9]+\\(?1:[=-]\\).*$")))
    ;; Grouped result.
    (looking-at "[0-9]+\\([=-]\\).*$")))

(defvar urgrep-mode-font-lock-keywords
  `(("^Urgrep started.*"
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
    ((lambda (limit)
       (unless (bolp) (forward-line))
       ;; Search line-by-line until we're looking at something that
       ;; looks like a context line.
       (catch 'found
         (while (< (point) limit)
           (when (urgrep-mode--looking-at-context-line)
             (goto-char (match-end 0))
             (throw 'found t))
           (forward-line)))
       ;; Only return non-nil if point is still within the limit.
       (< (point) limit))
     (0 'urgrep-context t)
     (2 `(face nil display ,(match-string 1)) nil t))
    ;; Hide excessive part of rgrep command.
    ("^find \\(\\(?:-H \\)?\\. -type d .*\\(?:\\\\)\\|\")\"\\)\\)"
     (1 (if grep-find-abbreviate grep-find-abbreviate-properties
          '(face nil abbreviated-command t))))))

(defvar urgrep--column-end-adjustment
  (if (< emacs-major-version 28) 0 1)
  "Handle core Emacs changes to the column range for `compile-mode' matches.
In Emacs 28+, the column range for matches is closed, but in previous
versions, it's half-open.  Use this to adjust the value as needed in
`urgrep--column-end'.  For more details on the change, see
<https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49624>.")

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
      (- mend beg urgrep--column-end-adjustment))))

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
       "\\(?1:[^\0\n]+\\)\\(?3:\0\\)\\(?2:[0-9]+\\)"
       "\\|"
       ;; Fallback if we can't use null terminators after the filename.
       ;; Use [1-9][0-9]* rather than [0-9]+ to allow ":0" in filenames.
       "\\(?1:[^\n]+?[^\n/]\\):\\(?2:[1-9][0-9]*\\)"
       "\\):")
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
  "Set up compilation variables for urgrep and run `urgrep-setup-hook'."
  (when-let ((tool-setup (urgrep--get-prop 'process-setup urgrep-current-tool)))
    (funcall tool-setup))
  (setq-local urgrep-num-matches-found 0
              compilation-exit-message-function #'urgrep-exit-message)
  (run-hooks 'urgrep-setup-hook))

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
        (goto-char beg)
        (while (re-search-forward "\033\\[35m\\(.*?\\)\033\\[0?m" end 1)
          (replace-match
           (propertize (match-string 1) 'face nil 'font-lock-face 'urgrep-hit
                       'urgrep-file-name t)
           t t))
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

(defun urgrep--start (command query tool &optional directory)
  "Start a urgrep process for COMMAND.
QUERY is the original argument list that generated COMMAND (or it may
be the same value as COMMAND).  TOOL is the tool that was used to
generate the command.  This sets `urgrep-current-query' and
`urgrep-current-tool' buffer-locally so that they can be used when
rerunning the search."
  (setq directory (if directory
                      (file-name-as-directory (expand-file-name directory))
                    default-directory))
  (with-current-buffer
      ;; Dynamically bind `urgrep-current-tool' so that `urgrep-process-filter'
      ;; can consult it.
      (urgrep--with-killed-local-variable 'urgrep-current-tool
        ;; Let-bind `default-directory' here so that the external command knows
        ;; where to search...
        (let ((urgrep-current-tool tool)
              (default-directory directory))
          (compilation-start command #'urgrep-mode)))
    ;; ... and then set `default-directory' here to be sure it's up to date.
    ;; This can get out of sync if re-running urgrep from a urgrep buffer, but
    ;; with a different search directory set.
    (setq default-directory directory)
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
If the region is active, return that.  Otherwise, return the symbol at
point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (when-let ((symbol (symbol-at-point)))
      (substring-no-properties (symbol-name symbol)))))

(defun urgrep--search-prompt (default)
  "Return the prompt to use when asking for the search query.
This depends on the current values of various urgrep options.  DEFAULT
indicates the default query, if any."
  (urgrep--format-prompt
   (concat
    "Search "
    (if urgrep-search-regexp "regexp" "string")
    (let ((block (append `(,#'pcase ',urgrep-context-lines)
                         urgrep--context-arguments)))
      (mapconcat (lambda (i) (concat " " i)) (eval block t) ""))
    (when urgrep-file-wildcards
      (format " in %s" (mapconcat #'identity urgrep-file-wildcards " "))))
   default))

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

(defun urgrep-set-file-wildcards (files)
  "Set the FILES (a wildcard or list thereof) to search.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive
   (let ((enable-recursive-minibuffers t))
                 (list (split-string (read-string "File wildcard: ")))))
  (setq urgrep-file-wildcards files)
  (when (window-minibuffer-p) (urgrep--update-search-prompt)))

(defvar urgrep-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\M-sr" #'urgrep-toggle-regexp)
    (define-key map "\M-sc" #'urgrep-toggle-case-fold)
    (define-key map "\M-sf" #'urgrep-set-file-wildcards)
    (define-key map "\M-sC" #'urgrep-set-context)
    (define-key map "\M-sB" #'urgrep-set-before-context)
    (define-key map "\M-sA" #'urgrep-set-after-context)
    map))

(cl-defun urgrep--read-query (initial &key tool (regexp urgrep-search-regexp)
                                      (case-fold urgrep-case-fold)
                                      (files urgrep-file-wildcards)
                                      (group urgrep-group-matches)
                                      (context urgrep-context-lines)
                                      (directory default-directory))
  "Prompt the user for a search query starting with an INITIAL value.
Return a list that can be passed to `urgrep-command' to turn into a shell
command.  TOOL, REGEXP, CASE-FOLD, FILES, GROUP, CONTEXT, and DIRECTORY
 are as in `urgrep-command'."
  (let* ((default-directory directory)
         (urgrep-search-regexp regexp)
         (urgrep-case-fold case-fold)
         (urgrep-file-wildcards files)
         (urgrep-context-lines context)
         (default (and (not initial) (urgrep--search-default)))
         (prompt (urgrep--search-prompt default))
         (query (minibuffer-with-setup-hook
                    (lambda () (setq-local urgrep--search-default default))
                  (read-from-minibuffer prompt initial urgrep-minibuffer-map nil
                                        'urgrep-search-history default)))
         (query (if (equal query "") default query)))
    (list query :tool (urgrep-get-tool tool) :regexp urgrep-search-regexp
          :case-fold urgrep-case-fold :files urgrep-file-wildcards :group group
          :context urgrep-context-lines :directory directory)))

(defun urgrep--read-command (command)
  "Read a shell command to use for searching, with initial value COMMAND."
  (read-shell-command "Search command: " command
                      (if (equal (car urgrep-command-history) command)
                          '(urgrep-command-history . 1)
                        'urgrep-command-history)))


;; User-facing functions (and supporting helpers)

(defun urgrep--read-directory (arg)
  "Get the directory to search in.
If ARG is nil, return the project's root directory.  If ARG's numeric
value is 4, return the current directory.  Otherwise, prompt for the
directory."
  (cond
   ((not arg) (let ((proj (project-current)))
                (if proj (urgrep--project-root proj) default-directory)))
   ((= (prefix-numeric-value arg) 4) default-directory)
   (t (read-directory-name "In directory: " nil nil t))))

;;;###autoload
(defun urgrep (query &rest rest)
  "Recursively search for a given QUERY.

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
Type \\[urgrep-set-after-context] to set the number of after context lines.
Type \\[urgrep-set-file-wildcards] to set a wildcard to filter the files searched."
  (interactive
   (let ((directory (urgrep--read-directory current-prefix-arg)))
     (urgrep--read-query nil :directory directory)))
  (let* ((full-query (cons query rest))
         (command (apply #'urgrep-command full-query))
         (tool (urgrep-get-tool (cadr (cl-member :tool full-query))))
         (directory (cadr (cl-member :directory full-query))))
    (urgrep--start command full-query tool directory)))

;;;###autoload
(defun urgrep-run-command (command directory tool)
  "Recursively search in DIRECTORY using the given COMMAND.

When called interactively, this behaves like `urgrep', but allows you
to edit the command before running it."
  (interactive
   (let* ((directory (urgrep--read-directory current-prefix-arg))
          (query (urgrep--read-query nil :directory directory)))
     (list (urgrep--read-command (apply #'urgrep-command query))
           directory (cadr (cl-member :tool query)))))
  (urgrep--start command command (urgrep-get-tool tool) directory))

;;;###autoload
(defun eshell/urgrep (&rest args)
  "Recursively search for a pattern, passing ARGS.
This is meant to be used as a command in Eshell."
  (require 'esh-cmd)
  (require 'em-unix)
  (eshell-eval-using-options
   "urgrep" args
   '(;; Regexp options
     (?G "basic-regexp" (bre) regexp
         "PATTERN is a basic regular expression")
     (?E "extended-regexp" (ere) regexp
         "PATTERN is an extended regular expression")
     (?P "perl-regexp" (pcre) regexp
         "PATTERN is a Perl-compatible regular expression")
     (?R "default-regexp" (t) regexp
         "PATTERN is a regular expression with the default syntax")
     (?F "fixed-strings" (nil) regexp
         "PATTERN is a string")
     ;; Case-folding options
     (?s "case-sensitive" nil case-fold
         "search case-sensitively for PATTERN")
     (?i "ignore-case" nil case-fold
         "ignore case when searching for PATTERN")
     (?S "smart-case" nil case-fold
         "ignore case when searching for PATTERN if PATTERN is all lower case")
     ;; Grouping options
     (nil "group" (t) group
          "group results by file")
     (nil "no-group" (nil) group
          "don't group results")
     ;; Context options
     (?C "context" t context-around
         "number of lines of context to print")
     (?B "before-context" t context-before
         "number of lines of leading context to print")
     (?A "after-context" t context-after
         "number of lines of trailing context to print")
     ;; General options
     (?h "help" nil nil "show this help message")
     :usage "[OPTION]... PATTERN [PATH]
Recursively search for PATTERN within PATH.")
   (unless args (error "Expected a search pattern"))
   ;; TODO: Add support for multiple paths.
   (when (> (length args) 2) (error "Only one path supported currently"))
   (let ((query (car args))
         (directory (or (cadr args) default-directory))
         (context
          (cond (context-around (string-to-number context-around))
                ((or context-before context-after)
                 (cons (if context-before (string-to-number context-before) 0)
                       (if context-after (string-to-number context-after) 0)))))
         options)
     ;; Fill the options to pass to `urgrep'.
     (when context (setq options (nconc `(:context ,context) options)))
     (when group (setq options (nconc `(:group ,(car group)) options)))
     (when case-fold (setq options (nconc `(:case-fold ,(car case-fold))
                                          options)))
     (when regexp (setq options (nconc `(:regexp ,(car regexp)) options)))
     ;; Run `urgrep'.
     (if (and (not eshell-plain-grep-behavior)
              (eshell-interactive-output-p)
              (not eshell-in-pipeline-p)
              (not eshell-in-subcommand-p))
         (apply #'urgrep query directory options)
       ;; Return the arguments to run directly.
       (when (not (equal directory default-directory))
         (error "Can't use plain urgrep with a non-default directory yet"))
       (setq options (nconc '(:color nil) options))
       (throw 'eshell-replace-command
              (let ((cmd (apply #'urgrep-command query options)))
                (eshell-parse-command (concat "*" cmd))))))))

(provide 'urgrep)
;;; urgrep.el ends here
