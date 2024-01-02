;;; urgrep.el --- Universal recursive grep -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Jim Porter
;; URL: https://github.com/jimporter/urgrep
;; Version: 0.4.0-git
;; Keywords: grep, search
;; Package-Requires: ((emacs "27.1") (compat "29.1.0.1") (project "0.3.0"))

;; This file is NOT part of GNU Emacs.

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
(require 'compat)
(require 'compile)
(require 'generator)
(require 'grep)
(require 'outline)
(require 'project)
(require 'shell)                        ; For `shell--parse-pcomplete-arguments'
(require 'text-property-search)
(require 'xref)                         ; For `xref--regexp-to-extended'

(defgroup urgrep nil
  "Run a grep-like command and display the results."
  :group 'tools
  :group 'processes)

(defcustom urgrep-abbreviate-command t
  "If non-nil, hide uninteresting parts of the command in the Urgrep buffer."
  :type 'boolean
  :group 'urgrep)

(defcustom urgrep-group-matches t
  "If non-nil, group matches by the file they were found in."
  :type 'boolean
  :group 'urgrep)

(defcustom urgrep-search-regexp nil
  "If non-nil, default to searching via regexp."
  :type 'boolean
  :group 'urgrep)

(defcustom urgrep-regexp-syntax 'bre
  "Default syntax to use for regexp searches.
Valid values are `bre' (basic regexp), `ere' (extended regexp),
or `prce' (Perl-compatible regexp)."
  :type '(radio (const :tag "Basic regexp" bre)
                (const :tag "Extended regexp" ere)
                (const :tag "Perl-compatible regexp" pcre))
  :group 'urgrep)

(defcustom urgrep-case-fold 'inherit
  "Default case-sensitivity for searches.
Valid values are nil (case-sensitive), t (case-insensitive), `smart'
\(case-insensitive if the query is all lower case), or `inherit'
\(case-sensitive if `case-fold-search' is nil, \"smart\" otherwise)."
  :type '(radio (const :tag "Case sensitive" nil)
                (const :tag "Smart case" smart)
                (const :tag "Inherit from `case-fold-search'" inherit)
                (const :tag "Case insensitive" t))
  :group 'urgrep)

(defcustom urgrep-search-hidden-files nil
  "If non-nil, default to searching in hidden files."
  :type 'boolean
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

;; `split-string-shell-command' was added in Emacs 28.1.
(defalias 'urgrep--split-string-shell-command
  (if (fboundp 'split-string-shell-command)
      #'split-string-shell-command
    (lambda (string)
      (with-temp-buffer
        (insert string)
        (let ((comint-file-name-quote-list shell-file-name-quote-list))
          (car (shell--parse-pcomplete-arguments)))))))

(defun urgrep--convert-regexp (expr from-syntax to-syntax)
  "Convert the regexp EXPR from FROM-SYNTAX to TO-SYNTAX."
  (cond ((and (not (eq from-syntax to-syntax))
              (or (eq from-syntax 'bre) (eq to-syntax 'bre)))
         ;; XXX: This is a bit of a hack, but xref.el contains an internal
         ;; function for converting between basic and extended regexps.  It
         ;; might be wise to use our own implementation, but this should work
         ;; for now.
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

(defun urgrep--maybe-shell-quote-argument (argument)
  "Quote ARGUMENT if needed for passing to an inferior shell.
This works as `shell-quote-argument', but avoids quoting unnecessarily
for MS shells."
  (declare-function w32-shell-dos-semantics "w32-fns" nil)
  (if (and (or (eq system-type 'ms-dos)
               (and (eq system-type 'windows-nt) (w32-shell-dos-semantics)))
           (not (string-match "[^-0-9a-zA-Z_./=]" argument)))
      argument
    (shell-quote-argument argument)))

(defun urgrep--shell-join (arguments)
  "Join ARGUMENTS by spaces, quoting each argument as needed for the shell."
  (mapconcat #'urgrep--maybe-shell-quote-argument arguments " "))

(defun urgrep--safe-file-name (file)
  "Get a safe form of FILE ready to pass to an external process.
This expands tildes and removes any remote host identifiers or quoting."
  (when-let ((remote (file-remote-p file)))
    (unless (equal remote (file-remote-p default-directory))
      (error "remote file doesn't match host for `default-directory'"))
    (setq file (file-local-name file)))
  (setq file (file-name-unquote file))
  (if (not (string-prefix-p "~" file))
      file
    (setq file (expand-file-name file))
    (if (file-in-directory-p file default-directory)
        (file-relative-name file)
      (file-local-name file))))

(defun urgrep--flatten-arguments (tree &optional abbrs)
  "Flatten a TREE of arguments into a single shell-quoted string.
This also finds sublists with the `:abbreviate' key and adds the
`abbreviated-command' text property to the resulting substring.

If ABBRS is non-nil, it should be a list of abbreviations to use,
one for each `:abbreviate' key found."
  (let (elems)
    (while (consp tree)
      (catch 'abbreviated
        (let ((elem (pop tree)))
          (while (consp elem)
            (when (eq (car elem) :abbreviate)
              (push (propertize
                     (urgrep--shell-join (flatten-list (cdr elem)))
                     'abbreviated-command (or (pop abbrs) t))
                    elems)
              (throw 'abbreviated t))
            (push (cdr elem) tree)
            (setq elem (car elem)))
          (when elem (push (urgrep--maybe-shell-quote-argument elem) elems)))))
    (when tree (push (urgrep--maybe-shell-quote-argument tree) elems))
    (string-join (nreverse elems) " ")))

(defun urgrep--interpolate-arguments (query tool props)
  "Interpolate search arguments for QUERY using TOOL.
PROPS is an alist of extra properties corresponding to the
properties defined in the `urgrep-tools' entry for TOOL."
  (let* ((arguments (urgrep--get-prop 'arguments tool))
         (abbrev (urgrep--get-prop 'abbreviations tool))
         (props `((executable . ,(urgrep--get-prop 'executable-name tool))
                  (query . ,query)
                  ,@(mapcar (pcase-lambda (`(,k . ,v))
                              (cons k (urgrep--get-prop-pcase
                                       k tool v "-arguments")))
                            props))))
    (urgrep--flatten-arguments (cl-sublis props arguments) abbrev)))

(rx-define urgrep-regular-number (seq (any "1-9") (* digit)))


;; Urgrep tools

(defconst urgrep--context-arguments
  '(((or '(0 . 0) 0) nil)
    (`(,b . 0) (list (format "-B%d" b)))
    (`(0 . ,a) (list (format "-A%d" a)))
    ((or `(,c . ,c) (and c (pred numberp))) (list (format "-C%d" c)))
    (`(,b . ,a) (list (format "-B%d" b) (format "-A%d" a)))))

(defun urgrep--add-grep-options (template extra-opts)
  "Add EXTRA-OPTS to the specified GREP template and return it.
This function changes match data."
  (unless (string-match "<C>" template)
    (error "grep template should have a <C> placeholder"))
  ;; Locally add options to the template that grep.el isn't aware of.
  (replace-match (concat "<C> " (urgrep--shell-join extra-opts))
                 t t template))

(cl-defun urgrep--rgrep-command (query &key tool regexp case-fold hidden
                                       file-wildcard root context color
                                       &allow-other-keys)
  "Get the command to run for QUERY when using rgrep.
Optional keys TOOL, REGEXP, CASE-FOLD, HIDDEN, FILE-WILDCARD,
ROOT, CONTEXT, and COLOR are as in `urgrep-command'."
  (grep-compute-defaults)
  (let ((case-fold-search nil)
        (grep-highlight-matches (if color 'always nil))
        (extra-opts
         (apply #'append
                (mapcar (pcase-lambda (`(,k . ,v))
                          (urgrep--get-prop-pcase k tool v "-arguments"))
                        `((context   . ,context)
                          (case-fold . ,case-fold)
                          (regexp    . ,regexp))))))
    (save-match-data
      (if root
          (let ((file-wildcard
                 (if file-wildcard (string-join file-wildcard " ") "*"))
                (dirs (urgrep--shell-join root))
                (grep-find-template (urgrep--add-grep-options
                                     grep-find-template extra-opts))
                (grep-find-ignored-directories grep-find-ignored-directories)
                (grep-find-ignored-files grep-find-ignored-files))
            (unless hidden
              ;; Ignore all dotfiles.
              (let ((pred (lambda (s) (not (string-prefix-p "." s)))))
                (setq grep-find-ignored-directories
                      (cons ".*" (seq-filter
                                  pred grep-find-ignored-directories))
                      grep-find-ignored-files
                      (cons ".*" (seq-filter pred grep-find-ignored-files)))))
            (let ((command (rgrep-default-command query file-wildcard dirs)))
              ;; Hide excessive part of rgrep command.
              (when (string-match (rx bol "find " (group (*? nonl)) " "
                                      (or "-exec" "-print"))
                     command)
                (put-text-property (match-beginning 1) (match-end 1)
                                   'abbreviated-command t command))
              command))
        ;; No ROOT, so do a regular `grep'.
        (grep-expand-template
         (urgrep--add-grep-options grep-template extra-opts) query)))))

(cl-defun urgrep--git-grep-command (query &key tool regexp case-fold hidden
                                          file-wildcard root group context
                                          color)
  "Get the command to run for QUERY when using git grep.
Optional keys TOOL, REGEXP, CASE-FOLD, HIDDEN, FILE-WILDCARD,
ROOT, CONTEXT, and COLOR are as in `urgrep-command'."
  (let ((pathspecs
         (cond
          ((equal root '("."))
           file-wildcard)
          ((and file-wildcard root)
           (mapcan
            (lambda (file)
              (mapcar (lambda (dir)
                        (concat ":(glob)" (file-name-concat dir "**" file)))
                      root))
            file-wildcard))
          (file-wildcard
           (error ":file-wildcard expects a non-nil :root"))
          (t root))))
    (urgrep--interpolate-arguments query tool
                                   `((regexp      . ,regexp)
                                     (case-fold   . ,case-fold)
                                     (hidden-file . ,hidden)
                                     (pathspec    . ,pathspecs)
                                     (group       . ,group)
                                     (context     . ,context)
                                     (color       . ,color)))))

(defun urgrep--rgrep-process-setup ()
  "Set up environment variables for rgrep.
See also `grep-process-setup'."
  ;; `setenv' modifies `process-environment' let-bound in `compilation-start'.
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
     (arguments executable (:abbreviate color "-rn" "--ignore-files")
                hidden-file file-wildcard group context case-fold regexp "-e"
                query root)
     (regexp-arguments ('bre  '("-G"))
                       ('ere  '("-E"))
                       ('pcre '("-P"))
                       (_     '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (hidden-file-arguments ((pred identity) '("--hidden")))
     (file-wildcard-arguments
      ((and x (pred identity))
       (mapcar (lambda (i) (concat "--include=" i)) x)))
     (root-arguments (x x))
     (group-arguments ((pred identity) '("--heading" "--break")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments
      ('nil '("--color=never"))
      (_    '("--color=always"
              "--colors=mt=01;31:fn=35:ln=:bn=:se=:sl=:cx=:ne"))))
    (ripgrep
     (executable-name . "rg")
     (regexp-syntax pcre)
     (arguments executable (:abbreviate color "-n") hidden-file file-wildcard
                group context case-fold regexp "--" query root)
     (regexp-arguments ('nil '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (hidden-file-arguments ((pred identity) '("--hidden")))
     (file-wildcard-arguments
      ((and x (pred identity))
       (flatten-list (mapcar (lambda (i) (cons "-g" i)) x))))
     (root-arguments (x x))
     (group-arguments ('nil '("--no-heading"))
                      (_    '("--heading")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments ('nil '("--color=never"))
                      (_    '("--color=always" "--colors=path:none"
                              "--colors=path:fg:magenta"
                              "--colors=line:none" "--colors=column:none"
                              "--colors=match:none" "--colors=match:fg:red"
                              "--colors=match:style:bold"))))
    (ag
     (executable-name . "ag")
     (regexp-syntax pcre)
     (arguments executable (:abbreviate color) hidden-file file-wildcard group
                context case-fold regexp "--" query root)
     (regexp-arguments ('nil '("-Q")))
     (case-fold-arguments ('nil '("-s"))
                          (_    '("-i")))
     (hidden-file-arguments ((pred identity) '("--hidden")))
     (file-wildcard-arguments
      ((and x (pred identity))
       (list "-G" (urgrep--wildcards-to-regexp x 'pcre))))
     (root-arguments ('(".") nil)
                     (x x))
     (group-arguments ('nil '("--nogroup"))
                      (_    '("--group")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments ('nil '("--nocolor"))
                      (_    '("--color" "--color-path=35" "--color-line="
                              "--color-match=1;31"))))
    (ack
     (executable-name . "ack")
     (regexp-syntax pcre)
     (arguments executable (:abbreviate color) hidden-file file-wildcard group
                context case-fold regexp "--" query root)
     (regexp-arguments ('nil '("-Q")))
     (case-fold-arguments ((pred identity) '("-i")))
     (hidden-file-arguments ('nil '("--ignore-dir=match:/^\\./"
                                    "--ignore-file=match:/^\\./")))
     (file-wildcard-arguments
      ((and x (pred identity))
       (list "-G" (urgrep--wildcards-to-regexp x 'pcre))))
     (root-arguments ('(".") nil)
                     (x x))
     (group-arguments ('nil '("--nogroup"))
                      (_    '("--group")))
     (context-arguments . ,urgrep--context-arguments)
     (color-arguments ('nil '("--nocolor"))
                      (_    '("--color" "--color-filename=magenta"
                              "--color-lineno=clear" "--color-colno=clear"
                              "--color-match=bold red"))))
    (git-grep
     (executable-name . "git")
     ;; XXX: Since we use --no-index, maybe it would make sense to allow using
     ;; git grep even outside of git repos.  However, that doesn't play nicely
     ;; with people who want to customize the arguments.
     (vc-backend . "Git")
     (regexp-syntax bre ere pcre)
     (command-function . ,#'urgrep--git-grep-command)
     (arguments executable (:abbreviate "--no-pager" color "--no-index"
                                        "--exclude-standard" "-n")
                group context case-fold regexp "-e" query "--" hidden-file
                pathspec)
     (abbreviations "grep")
     (regexp-arguments ('bre  '("-G"))
                       ('ere  '("-E"))
                       ('pcre '("-P"))
                       (_     '("-F")))
     (case-fold-arguments ((pred identity) '("-i")))
     (hidden-file-arguments ('nil '(":!.*")))
     (pathspec-arguments (x x))
     (group-arguments ((pred identity) '("--heading" "--break")))
     (context-arguments . ,urgrep--context-arguments)
     ;; git is a bit odd in that color specification happens *before* the
     ;; subcommand and turning colors on/off happens *after*, so
     ;; `color-arguments' needs to include the subcommand "grep".
     (color-arguments ('nil '("grep" "--no-color"))
                      (_    '("-c" "color.grep.filename=magenta"
                              "-c" "color.grep.match=bold red"
                              "-c" "color.grep.context="
                              "-c" "color.grep.function="
                              "-c" "color.grep.lineNumber="
                              "-c" "color.grep.column="
                              "-c" "color.grep.selected="
                              "-c" "color.grep.separator="
                              "grep" "--color"))))
    (grep
     ;; Note: We only use these for detecting the usability of find/grep.  To
     ;; modify the programs that actually run, change `grep-find-template'.
     (executable-name "find" "grep")
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

(defvar urgrep--cached-tool nil
  "The cached urgrep tool to use.
This value is connection-local.")

(connection-local-set-profile-variables
 'urgrep-connection-local-profile
 '((urgrep--cached-tool . nil)))

(connection-local-set-profiles
 '(:application tramp)
 'urgrep-connection-local-profile)

(defcustom urgrep-preferred-tools nil
  "List of urgrep tools to search for.
This can be nil to use the default list of tools in `urgrep-tools'
or a list of tools to try in descending order of preference.  Each
tool can be either a symbol naming the tool or a cons cell of the
tool name and the file name of the executable (or a list thereof
if there are multiple executables)."
  :type `(choice
          (const :tag "Default" nil)
          (repeat :tag "List of tools"
                  ,(let* ((tool-names (mapcar (lambda (i) `(const ,(car i)))
                                              urgrep-tools))
                          (tool-choice `(choice :tag "Tool" . ,tool-names)))
                     (append tool-choice
                             `((cons :tag "(tool . path)"
                                     ,tool-choice (string :tag "Path")))))))
  :set (lambda (symbol value)
         (setq urgrep--cached-tool nil)
         (set-default symbol value))
  :group 'urgrep)

(defsubst urgrep-connection-local-profile ()
  "Get a connection-local profile name for urgrep."
  (intern (concat "urgrep-connection-local-profile-"
		  (or (file-remote-p default-directory) "local"))))

(defun urgrep--get-prop (prop tool &optional suffix)
  "Get the property PROP from TOOL, or nil if PROP is undefined.
If SUFFIX is non-nil, append it to PROP to generate the property name."
  (when suffix
    (setq prop (intern (concat (symbol-name prop) suffix))))
  (alist-get prop (cdr tool)))

(defun urgrep--get-prop-pcase (prop tool value &optional suffix)
  "Get the property PROP from TOOL and use it as a `pcase' macro for VALUE.
If SUFFIX is non-nil, append it to PROP to generate the property
name."
  (when-let ((cases (urgrep--get-prop prop tool suffix))
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
  (with-connection-local-variables
   (let ((use-cache (equal urgrep-preferred-tools
                           (default-value 'urgrep-preferred-tools)))
         vc-backend-name)
     (if (and urgrep--cached-tool use-cache)
         urgrep--cached-tool
       (catch 'found
         (iter-do (tool (urgrep--iter-tools))
           (let ((tool-executable (urgrep--get-prop 'executable-name tool))
                 (tool-vc-backend (urgrep--get-prop 'vc-backend tool)))
             ;; If we see a VC-specific tool, this host might use different
             ;; tools for different directories, so we can't cache anything.
             (setq use-cache (and use-cache (not tool-vc-backend)))
             ;; Cache the VC backend name if we need it.
             (when-let (((and tool-vc-backend (not vc-backend-name)))
                        (proj (project-current)))
               (setq vc-backend-name (vc-responsible-backend
                                      (project-root proj))))
             ;; If we find the executable (and it's for the right VC backend, if
             ;; relevant), cache it if possible and then return it.
             (when (and (seq-every-p (lambda (i) (executable-find i t))
                                     (ensure-list tool-executable))
                        (or (not tool-vc-backend)
                            (string= vc-backend-name tool-vc-backend)))
               (when use-cache
                 (setq urgrep--cached-tool tool)
                 (when (file-remote-p default-directory)
                   (connection-local-set-profile-variables
                    (urgrep-connection-local-profile)
                    `((urgrep--cached-tool . ,urgrep--cached-tool)))
                   (connection-local-set-profiles
                    (connection-local-criteria-for-default-directory)
                    (urgrep-connection-local-profile))))
               (throw 'found tool)))))))))

(defun urgrep-get-tool (&optional tool)
  "Get the urgrep tool for TOOL.
If TOOL is nil, get the default tool.  If TOOL is a symbol, look
it up in `urgrep-tools'.  Otherwise, return TOOL as-is."
  (pcase tool
    ('nil (urgrep--get-default-tool))
    ((and (pred symbolp) tool) (assq tool urgrep-tools))
    (tool tool)))

(defun urgrep--guess-tool (command)
  "Guess the urgrep tool from the specified COMMAND."
  (catch 'found
    (when-let ((args (urgrep--split-string-shell-command
                      ;; First, split by semicolon, since
                      ;; `split-string-shell-command' only returns the *last*
                      ;; command.
                      (car (split-string command ";"))))
               (command-name (file-name-nondirectory (car args))))
      (dolist (tool urgrep-tools)
        (when (string= command-name
                       (car (ensure-list (urgrep--get-prop
                                          'executable-name tool))))
          (throw 'found tool))))
    (error "Unable to guess urgrep tool from command")))

(defun urgrep--get-best-syntax (syntax tool)
  "Return the regexp syntax closest to SYNTAX that TOOL supports."
  (let ((tool-syntaxes (urgrep--get-prop 'regexp-syntax tool)))
    (cond ((not syntax) nil)
          ((memq syntax tool-syntaxes) syntax)
          ((and (eq syntax 'ere) (memq 'pcre tool-syntaxes)) 'pcre)
          ((and (eq syntax 'pcre) (memq 'extended tool-syntaxes)) 'ere)
          (t (car tool-syntaxes)))))

;;;###autoload
(cl-defun urgrep-command (query &key tool regexp (case-fold 'inherit) hidden
                                file-wildcard (root ".") (group t) (context 0)
                                (color t))
  "Return a command to use to search for QUERY.
Several keyword arguments can be supplied to adjust the resulting
command:

TOOL: a tool from `urgrep-tools': a key-value pair from the list,
just the key, or nil to use the default tool.

REGEXP: the style of regexp to use for results; one of nil (fixed
strings), `bre' (basic regexp), `ere' (extend regexp), `pcre'
\(Perl-compatible regexp), or t (the default regexp style stored
in `urgrep-regexp-syntax').

CASE-FOLD: determine whether QUERY is case-sensitive or not;
possible values are as `urgrep-case-fold', defaulting to
`inherit'.

HIDDEN: non-nil to search in hidden files; defaults to nil.

FILE-WILDCARD: a wildcard (or list of wildcards) to limit the
files searched.

ROOT: a file/directory (or list thereof) to search in; by
default, search within `default-directory'.  If nil, don't search
anywhere; this is useful when making a template command, e.g. to
use with \"xargs\".

GROUP: show results grouped by filename (t, the default), or if
nil, prefix the filename on each result line.

CONTEXT: the number of lines of context to show around results;
either an integer (to show the same number of lines before and
after) or a cons (to show CAR and CDR lines before and after,
respectively).

COLOR: non-nil (the default) if the output should use color."
  (with-connection-local-variables
   (let* ((regexp-syntax (if (eq regexp t) urgrep-regexp-syntax regexp))
          (file-wildcard (ensure-list file-wildcard))
          (root (mapcar #'urgrep--safe-file-name (ensure-list root)))
          (tool (or (urgrep-get-tool tool)
                    (error "unknown tool %s" tool)))
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
         (funcall cmd-fun query :tool tool :regexp tool-re-syntax
                  :case-fold case-fold :hidden hidden
                  :file-wildcard file-wildcard :root root
                  :group group :context context :color color)
       (urgrep--interpolate-arguments query tool
                                      `((regexp        . ,tool-re-syntax)
                                        (case-fold     . ,case-fold)
                                        (hidden-file   . ,hidden)
                                        (file-wildcard . ,file-wildcard)
                                        (root          . ,root)
                                        (group         . ,group)
                                        (context       . ,context)
                                        (color         . ,color)))))))


;; urgrep-mode

(defvar outline-search-function)
(defvar outline-level)
(defvar outline-minor-mode-use-buttons)

(declare-function outline-cycle "outline" (&optional event))
(declare-function outline-search-text-property "outline"
                  (property &optional value bound move backward looking-at))

(defvar urgrep-file-wildcards nil
  "Zero or more wildcards to limit the files searched.")
(defvar urgrep-num-matches-found 0
  "Running total of matches found.  This will be set buffer-locally.")
(defvar-local urgrep-current-query nil
  "The most recent search query run in this buffer.")
(defvar-local urgrep-current-tool nil
  "The most recent search tool used in this buffer.")

(defvar-local urgrep--filter-start nil
  "The in-buffer position to start `urgrep-filter'.")
(defvar-local urgrep--filter-last-file nil
  "The previously-found file name in `urgrep-filter'.")

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

(defvar-keymap urgrep-mode-map
  ;; Don't inherit from `compilation-minor-mode-map', because that introduces a
  ;; menu bar item we don't want.
  :parent special-mode-map
  "<mouse-2>"     #'compile-goto-error
  "<follow-link>" 'mouse-face
  "C-c C-c"       #'compile-goto-error
  "C-m"           #'compile-goto-error
  "C-o"           #'compilation-display-error
  "C-c C-k"       #'kill-compilation
  "M-n"           #'compilation-next-error
  "M-p"           #'compilation-previous-error
  "M-{"           #'compilation-previous-file
  "M-}"           #'compilation-next-file
  "n"             #'next-error-no-select
  "p"             #'previous-error-no-select
  "{"             #'compilation-previous-file
  "}"             #'compilation-next-file
  "TAB"           #'compilation-next-error
  "<backtab>"     #'compilation-previous-error
  "g"             #'urgrep-search-again)

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
  ;; When bootstrapping, `tool-bar-map' is not properly initialized yet, so
  ;; don't do anything.
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

(defvar-keymap urgrep-mode-abbreviation-map
  "<down-mouse-2>" #'mouse-set-point
  "<mouse-2>"      #'grep-find-toggle-abbreviation
  "C-m"            #'grep-find-toggle-abbreviation)

(defconst urgrep-mode-line-matches
  `(" [" (:propertize (:eval (int-to-string urgrep-num-matches-found))
                      face urgrep-match-count
                      help-echo "Number of matches so far")
    "]"))

(defun urgrep-mode--looking-at-context-line ()
  "Return t if looking at a grep-like context line.

If so, this function sets the match data, with the first match
group indicating the separator between the line number and the
match, and the second group indicating the separator between the
file name and line number."
  ;; Use the `urgrep-file-name' property set by `urgrep-filter' to reliably
  ;; detect if the result was printed in grouped or ungrouped format.
  (if (get-text-property (point) 'urgrep-file-name)
      ;; Ungrouped result.
      (let* ((file-name-end (next-single-property-change
                             (point) 'urgrep-file-name))
             (file-name (buffer-substring-no-properties (point) file-name-end)))
        (looking-at (rx (literal file-name)
                        (or (group-n 2 "\0") (any ":=-"))
                        (+ digit) (group-n 1 (any "=-"))
                        (* nonl) eol)))
    ;; Grouped result.
    (looking-at (rx (+ digit) (group (any "=-")) (* nonl) eol))))

(defvar urgrep-mode-font-lock-keywords
  `((,(rx bol "Urgrep started" (* nonl))
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    (,(rx bol "Urgrep finished with "
          (or (group (? (+ digit) " ") (or "match" "matches") " found")
              (group "no matches found"))
          (* nonl))
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'urgrep-match-count nil t)
     (2 'compilation-warning nil t))
    (,(rx bol "Urgrep "
          (group (or "exited abnormally" "interrupt" "killed" "terminated"))
          (? (* nonl) " with code " (group (+ digit)))
          (* nonl))
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ;; Highlight context lines of various flavors.
    ((lambda (limit)
       (unless (bolp) (forward-line))
       ;; Search line-by-line until we're looking at something that looks like a
       ;; context line.
       (catch 'found
         (while (< (point) limit)
           (when (urgrep-mode--looking-at-context-line)
             (goto-char (match-end 0))
             (throw 'found t))
           (forward-line)))
       ;; Only return non-nil if point is still within the limit.
       (< (point) limit))
     (0 'urgrep-context t)
     (2 `(face nil display ,(match-string 1)) nil t))))

(defvar urgrep--column-end-adjustment
  (if (< emacs-major-version 28) 0 1)
  "Handle core Emacs changes to the column range for `compile-mode' matches.
In Emacs 28+, the column range for matches is closed, but in
previous versions, it's half-open.  Use this to adjust the value
as needed in `urgrep--column-end'.

For more details on the change, see
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
        (buffer-substring-no-properties (prop-match-beginning match)
                                        (prop-match-end match))
      ;; Emacs 27 and lower will break if we return nil from this function.
      (when (< emacs-major-version 28) "*unknown*"))))

(defconst urgrep-regexp-alist
  ;; XXX: Try to rely on ANSI escapes as with the match highlight?
  `(;; Ungrouped matches
    (,(rx bol
          (or ;; Parse using a null terminator after the filename when possible.
              (seq (group-n 1 (+ (not (any "\0" "\n"))))
                   (group-n 3 "\0") (group-n 2 (+ digit)))
              ;; Fallback if we can't use null terminators after the filename.
              ;; Require line numbers to start with a nonzero digit to allow
              ;; ":0" in filenames.
              (seq (group-n 1 (+? nonl) (not (any "\n" "/")))
                   ":" (group-n 2 urgrep-regular-number)))
          ":")
     1 2 (,#'urgrep--column-begin . ,#'urgrep--column-end)
     nil nil
     (3 '(face nil display ":")))

    ;; Grouped matches
    (,(rx bol (group urgrep-regular-number) ":")
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
      ;; This relies on the fact that `compilation-start' sets buffer-modified
      ;; to nil before running the command, so the buffer is still unmodified if
      ;; there is no output.
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
  (unless urgrep--filter-start
    (setq urgrep--filter-start compilation-filter-start))
  (save-excursion
    (rx-let ((ansi-cseq (&rest rest) (seq "\033[" rest))
             (ansi-any-cseq (ansi-cseq (*? anything) (any "@-~")))
             (ansi-sgr (&rest rest) (ansi-cseq rest "m")))
      (let ((end (point)))
        (goto-char urgrep--filter-start)
        (catch 'done
          (while (search-forward "\033" end 'limit)
            (when (eq (char-after) ?\[)
              (backward-char)
              (unless (looking-at (rx ansi-any-cseq))
                ;; This control sequence is incomplete.  Try again next time.
                (throw 'done nil))
              (cond
               ;; Delete "erase in line" ANSI CSI sequence.
               ((re-search-forward (rx point (ansi-cseq (* digit) "K")) end t)
                (replace-match "" t t))
               ;; Highlight matching filenames and delete ANSI SGR escapes.
               ((re-search-forward (rx bol point (ansi-sgr "35")
                                       (group (*? anything))
                                       (ansi-sgr (? "0")))
                                   end t)
                (let* ((file-name (match-string 1))
                       (same-file (equal file-name urgrep--filter-last-file)))
                  (replace-match
                   (propertize file-name 'face nil
                               'font-lock-face 'urgrep-hit
                               'urgrep-file-name (if same-file 'repeat 'first))
                   t t)
                  (setq urgrep--filter-last-file file-name)))
               ;; Highlight matches and delete ANSI SGR escapes.
               ((re-search-forward (rx point
                                       (or ;; Find the escapes together...
                                        (ansi-sgr (or "01" "1") ";31")
                                        ;; ... or apart.
                                        (seq (ansi-sgr (or "01" "1"))
                                             (ansi-sgr "31")))
                                       ;; Matches always stop by end of line.
                                       ;; (Multi-line matches get restarted
                                       ;; after the line number is printed.)
                                       (group (*? nonl) (? "\n"))
                                       (ansi-sgr (? "0")))
                                   end t)
                (replace-match
                 (propertize (match-string 1) 'face nil
                             'font-lock-face 'urgrep-match)
                 t t)
                (cl-incf urgrep-num-matches-found))
               ;; Delete no-op ANSI SGR escapes.
               ((re-search-forward (rx point (ansi-sgr (? "0"))
                                       (group (*? anything))
                                       (ansi-sgr (? "0")))
                                   end t)
                (replace-match (match-string 1) t t))
               ;; If nothing matched, try again next time.
               (t (throw 'done nil))))))
        (setq urgrep--filter-start (point))))))

(defun urgrep-outline-search (&optional bound move backward looking-at)
  "Search for outline headings.  See `outline-search-function'."
  (outline-search-text-property 'urgrep-file-name 'first bound move backward
                                looking-at))

(defun urgrep-goto-match-or-outline-cycle (&optional event)
  "Visit the source for the match at point or cycle outline visibility.
If non-nil, EVENT should be a mouse event."
  (interactive (list last-input-event))
  (condition-case nil
      (compile-goto-error event)
    (error (outline-cycle event))))

(define-compilation-mode urgrep-mode "Urgrep"
  "A compilation mode for various grep-like tools."
  (setq-local tool-bar-map urgrep-mode-tool-bar-map
              compilation-process-setup-function #'urgrep-process-setup
              compilation-error-face 'urgrep-hit
              compilation-error-regexp-alist urgrep-regexp-alist
              compilation-mode-line-errors urgrep-mode-line-matches
              compilation-disable-input t
              compilation-error-screen-columns nil
              outline-search-function #'urgrep-outline-search
              outline-level (lambda () 1)
              outline-minor-mode-use-buttons 'in-margins
              urgrep--filter-start nil
              urgrep--filter-last-file nil)
  ;; Locally override the button map for `outline-minor-mode'.
  (when (boundp 'outline-overlay-button-map)
    (setq-local outline-overlay-button-map
                (define-keymap
                  :parent outline-overlay-button-map
                  "RET" #'urgrep-goto-match-or-outline-cycle)))
  (add-hook 'compilation-filter-hook #'urgrep-filter nil t))

(defun urgrep--hide-abbreviations (command)
  "If `urgrep-abbreviate-command' is non-nil, hide abbreviations in COMMAND."
  (when urgrep-abbreviate-command
    (let ((ellipsis (if (char-displayable-p ?…) "…" "..."))
          (start 0) end)
      (while start
        (setq end (next-single-property-change
                   start 'abbreviated-command command))
        (when-let ((abbrev (get-text-property start 'abbreviated-command
                                              command)))
          (add-text-properties
           start end
           `( face nil
              display ,(format "[%s%s]" (if (eq abbrev t) "" abbrev) ellipsis)
              mouse-face highlight
              help-echo "RET, mouse-2: show unabbreviated command"
              keymap ,urgrep-mode-abbreviation-map)
           command))
        (setq start end))))
  command)

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
      ;; Run this in a temporary buffer to ensure that we let-bind the default
      ;; binding for `urgrep-current-tool'.  `urgrep-process-setup' needs to
      ;; consult this let-binding from another buffer.
      (with-temp-buffer
        ;; Let-bind `default-directory' here so that the external command knows
        ;; where to search...
        (let ((urgrep-current-tool tool)
              (default-directory directory))
          (compilation-start (urgrep--hide-abbreviations command)
                             #'urgrep-mode)))
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
  (format-prompt
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

(defun urgrep-set-tool (tool)
  "Set the TOOL to use for the current search.
TOOL should be a symbol corresponding to one of the keys in
`urgrep-tools'."
  (interactive
   (let* ((enable-recursive-minibuffers t)
          (tool-string (completing-read
                        (format-prompt "Tool" (car urgrep-current-tool))
                        (mapcar (lambda (i) (symbol-name (car i)))
                                urgrep-tools)
                        nil 'require-match nil nil
                        (symbol-name (car urgrep-current-tool)))))
     (list (unless (string= tool-string "") (intern tool-string)))))
  (when tool
    (setq-default urgrep-current-tool (urgrep-get-tool tool))
    (message "using %s" tool)))

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

(defun urgrep-toggle-search-hidden-files ()
  "Toggle whether or not to search in hidden files.
Within the `urgrep' search prompt, this sets the value only for the
current search.  Outside the prompt, this sets the value for all
future searches."
  (interactive)
  (message (if (setq urgrep-search-hidden-files
                     (not urgrep-search-hidden-files))
               "search hidden"
             "exclude hidden")))

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

(defvar-keymap urgrep-minibuffer-map
  :parent minibuffer-local-map
  "M-s r" #'urgrep-toggle-regexp
  "M-s c" #'urgrep-toggle-case-fold
  "M-s f" #'urgrep-set-file-wildcards
  "M-s h" #'urgrep-toggle-search-hidden-files
  "M-s C" #'urgrep-set-context
  "M-s B" #'urgrep-set-before-context
  "M-s A" #'urgrep-set-after-context
  "M-s t" #'urgrep-set-tool)

(cl-defun urgrep--read-query (initial &key tool (regexp urgrep-search-regexp)
                                      (case-fold urgrep-case-fold)
                                      (hidden urgrep-search-hidden-files)
                                      (file-wildcard urgrep-file-wildcards)
                                      (group urgrep-group-matches)
                                      (context urgrep-context-lines)
                                      (default (and (not initial)
                                                    (urgrep--search-default))))
  "Prompt the user for a search query starting with an INITIAL value.
Return a list that can be passed to `urgrep-command' to turn into
a shell command.  TOOL, REGEXP, CASE-FOLD, FILES, GROUP, and
CONTEXT are as in `urgrep-command'.

In addition, you can pass DEFAULT to specify the default search
query.  If nil, guess the default based on the current region or
point."
  ;; Get the tool in the current buffer, in case `urgrep-preferred-tools' has a
  ;; buffer-local value.
  (setq tool (urgrep-get-tool tool))
  ;; Run this in a temporary buffer to make sure that none of the dynamic
  ;; variables below that we let-bind have buffer-local bindings.  If they did,
  ;; we wouldn't be able to retrieve the values for them that we set from inside
  ;; the minibuffer.
  (with-temp-buffer
    (let* ((urgrep-current-tool tool)
           (urgrep-search-regexp regexp)
           (urgrep-case-fold case-fold)
           (urgrep-search-hidden-files hidden)
           (urgrep-file-wildcards file-wildcard)
           (urgrep-context-lines context)
           (prompt (urgrep--search-prompt default))
           (query (minibuffer-with-setup-hook
                      (lambda () (setq-local urgrep--search-default default))
                    (read-from-minibuffer prompt initial urgrep-minibuffer-map
                                          nil 'urgrep-search-history default)))
           (query (if (equal query "") default query)))
      (list query :tool urgrep-current-tool :regexp urgrep-search-regexp
            :case-fold urgrep-case-fold :hidden urgrep-search-hidden-files
            :file-wildcard urgrep-file-wildcards :group group
            :context urgrep-context-lines))))

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
                (if proj (project-root proj) default-directory)))
   ((= (prefix-numeric-value arg) 4) default-directory)
   (t (read-directory-name "In directory: " nil nil t))))

;;;###autoload
(defun urgrep (query &rest rest)
  "Recursively search for a given QUERY.

When called interactively, search in the project's root directory, or
the current directory if there is no current project.  With \
\\[universal-argument] prefix,
search in the current directory.  With two \\[universal-argument] prefixes, \
prompt for a
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
Type \\[urgrep-toggle-search-hidden-files] to toggle searching in hidden files.
Type \\[urgrep-set-file-wildcards] to set a wildcard to filter the files \
searched."
  (interactive
   (let ((default-directory (urgrep--read-directory current-prefix-arg)))
     `(,@(urgrep--read-query nil) :default-directory ,default-directory)))
  (let* ((tool (urgrep-get-tool (plist-get rest :tool)))
         (directory (prog1 (plist-get rest :default-directory)
                      (cl-remf rest :default-directory)))
         (full-query (cons query rest))
         (command (apply #'urgrep-command full-query)))
    (urgrep--start command full-query tool directory)))

;;;###autoload
(cl-defun urgrep-run-command (command &key ((:default-directory directory))
                                      tool)
  "Recursively search using the given COMMAND.

TOOL is the search tool corresponding to COMMAND (see
`urgrep-get-tool').  If nil, guess the tool based on the value of
COMMAND.

When called interactively, this behaves like `urgrep', but allows you
to edit the command before running it.

\(fn COMMAND &key TOOL)"
  (interactive
   (let* ((default-directory (urgrep--read-directory current-prefix-arg))
          (query (urgrep--read-query nil))
          (command (urgrep--read-command (apply #'urgrep-command query)))
          (tool (condition-case nil (urgrep--guess-tool command)
                  (error (plist-get (cdr query) :tool)))))
     (list command :default-directory default-directory :tool tool)))
  (let ((tool (if tool (urgrep-get-tool tool)
                (urgrep--guess-tool command))))
    (urgrep--start command command tool directory)))

(cl-eval-when (compile)
  (require 'esh-cmd)
  (require 'esh-opt))

;;;###autoload
(eval-after-load 'esh-cmd '(add-to-list 'eshell-complex-commands "urgrep"))

;;;###autoload
(defun eshell/urgrep (&rest args)
  "Recursively search for a pattern, passing ARGS.
This is meant to be used as a command in Eshell."
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
     ;; Hidden file options
     (nil "hidden" (t) hidden
          "search hidden files")
     (nil "no-hidden" (nil) hidden
          "don't search hidden files")
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
     :usage "[OPTION]... PATTERN [ROOT]...
Recursively search for PATTERN within ROOT.")
   (unless args (error "Expected a search pattern"))
   (let* ((query (car args))
          (root (cdr args))
          (context
           (cond
            (context-around (string-to-number context-around))
            ((or context-before context-after)
             (cons (if context-before (string-to-number context-before) 0)
                   (if context-after (string-to-number context-after) 0)))))
          options)
     ;; Fill the options to pass to `urgrep'.
     (when root      (setq options `(:root      ,root            . ,options)))
     (when context   (setq options `(:context   ,context         . ,options)))
     (when hidden    (setq options `(:hidden    ,(car hidden)    . ,options)))
     (when group     (setq options `(:group     ,(car group)     . ,options)))
     (when case-fold (setq options `(:case-fold ,(car case-fold) . ,options)))
     (when regexp    (setq options `(:regexp    ,(car regexp)    . ,options)))
     ;; Run `urgrep'.
     (if (and (not (bound-and-true-p eshell-plain-grep-behavior))
              (eshell-interactive-output-p)
              (not eshell-in-pipeline-p)
              (not eshell-in-subcommand-p))
         (apply #'urgrep query options)
       ;; Return the arguments to run directly.
       (unless (eshell-interactive-output-p)
         (setq options `(:color nil . ,options)))
       (throw 'eshell-replace-command
              (let* (;; Ensure we generate a POSIX shell-like command so that
                     ;; Eshell can (hopefully) parse it correctly.
                     (system-type 'gnu/linux)
                     (cmd (apply #'urgrep-command query options)))
                (eshell-parse-command (concat "*" cmd))))))))

(provide 'urgrep)
;;; urgrep.el ends here
