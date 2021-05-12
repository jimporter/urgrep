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

;; A universal frontend to various grep-like tools. Currently, only ag and grep
;; are supported.

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

(defface urgrep-hit '((t :inherit compilation-info))
  "Face for matching files."
  :group 'urgrep)

(defface urgrep-match-count '((t :inherit compilation-info))
  "Face for match counts."
  :group 'urgrep)

(defface urgrep-match '((t :inherit match))
  "Face for matching text."
  :group 'urgrep)

(defvar urgrep-search-history nil "History list for urgrep.")
(defvar urgrep-num-matches-found 0
  "Running total of matches found. This will be set buffer-locally.")

;; Set the first column to 0 because that's how we currently count.
;; XXX: It might be worth changing this to 1 if we allow reading the column
;; number explicitly in the output.
(defvar urgrep-first-column 0)

(defun urgrep-rgrep--command (query)
  (grep-compute-defaults)
  (rgrep-default-command query "*" nil))

(defvar urgrep-tools
  `(("ag"
     (executable-name "ag")
     (always-arguments ("--color-match" "1;31")))
    ("grep"
     (executable-name "grep")
     (command-function ,#'urgrep-rgrep--command)))
  "An alist of known tools to try when running urgrep.")

(defun urgrep-get-property (tool prop)
  "Get a given property PROP from TOOL, or nil if PROP is undefined."
  (when-let ((prop-entry (assoc prop (cdr tool))))
    (cadr prop-entry)))

(defun urgrep-get-tool ()
  "Get the preferred urgrep tool from `urgrep-tools'."
  (cl-dolist (i urgrep-tools)
    (when (executable-find (urgrep-get-property i 'executable-name) t)
      (cl-return i))))

(defvar urgrep-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    map)
  "Keymap for urgrep buffers.
`compilation-minor-mode-map' is a parent of this.")

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
     ;; Hide excessive part of rgrep command
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
       "\\(?1:"
       "\\(?:[a-zA-Z]:\\)?" ; Allow "C:..." for w32.
       "[^\n:]+?[^\n/:]"
       "\\)"
       ":[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:"
       "\\)")
     1 2 (,#'urgrep--column-begin . ,#'urgrep--column-end)
     nil nil
     (3 '(face nil display ":")))

    ;; Grouped matches
    ("^\\([[:digit:]]+\\):"
     ,#'urgrep--grouped-filename 1
     (,#'urgrep--column-begin . ,#'urgrep--column-end)))
  "Regexp used to match results.
See `compilation-error-regexp-alist' for format details.")

(defun urgrep-command (query &optional tool)
  (let* ((tool (or tool (urgrep-get-tool)))
         (cmd-fun (urgrep-get-property tool 'command-function)))
    (if cmd-fun
        (funcall cmd-fun query)
      (let ((arguments (or (urgrep-get-property tool 'always-arguments) '())))
        (setq arguments (cons (if urgrep-group-matches "--group" "--nogroup")
                              arguments))
        ;; FIXME: Inside compile and dired buffers, `shell-quote-argument'
        ;; doesn't handle TRAMP right...
        (mapconcat #'shell-quote-argument
                   (append '("ag") arguments `(,query))
                   " ")))))

(defun urgrep-process-setup ()
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
        (while (re-search-forward "\033\\[0?1;31m\\(.*?\\)\033\\[0?m" end 1)
          (replace-match
           (propertize (match-string 1) 'face nil 'font-lock-face 'urgrep-match)
           t t)
          (cl-incf urgrep-num-matches-found))
        ;; Highlight matching filenames and delete ANSI escapes.
        (when urgrep-group-matches
          (goto-char beg)
          (while (re-search-forward "\033\\[1;32m\\(.*?\\)\033\\[0?m" end 1)
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
  (setq-local compilation-process-setup-function 'urgrep-process-setup
              compilation-error-face 'urgrep-hit
              compilation-error-regexp-alist urgrep-regexp-alist
              compilation-mode-line-errors urgrep-mode-line-matches
              compilation-disable-input t
              compilation-error-screen-columns nil)
  (add-hook 'compilation-filter-hook 'urgrep-filter nil t))

(defun urgrep--read-directory (arg)
  (cond
   ((not arg) (let ((proj (project-current)))
                (if proj (project-root proj) default-directory)))
   ((= (prefix-numeric-value arg) 4) default-directory)
   (t (read-directory-name "In directory: " nil nil t))))

;;;###autoload
(defun urgrep (query &optional directory)
  "Search in DIRECTORY for a given QUERY."
  (interactive
   (list (read-from-minibuffer "Search for: " "" nil nil 'urgrep-search-history)
         (urgrep--read-directory current-prefix-arg)))
  (let ((default-directory (or directory default-directory)))
    (compilation-start (urgrep-command query) 'urgrep-mode)))

(provide 'urgrep)

;;; urgrep.el ends here
