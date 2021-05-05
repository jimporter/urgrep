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

;; A universal frontend to various grep-like tools. Currently, only ag is
;; supported.

;;; Code:

(require 'compile)
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

(defface urgrep-hit-face '((t :inherit compilation-info))
  "Face for matching files."
  :group 'urgrep)

(defface urgrep-match-count-face '((t :inherit compilation-info))
  "Face for match counts."
  :group 'urgrep)

(defface urgrep-match-face '((t :inherit match))
  "Face for matching text."
  :group 'urgrep)

(defvar urgrep-search-history nil "History list for urgrep.")
(defvar urgrep-num-matches-found 0
  "Running total of matches found. This will be set buffer-locally.")

;; Set the first column to 0 because that's how we currently count.
;; XXX: It might be worth changing this to 1 if we allow reading the column
;; number explicitly in the output.
(defvar urgrep-first-column 0)

(defconst urgrep-mode-line-matches
  `(" [" (:propertize (:eval (int-to-string urgrep-num-matches-found))
                      face 'urgrep-match-count-face
                      help-echo "Number of matches so far")
    "]"))

(defvar urgrep-mode-font-lock-keywords
   '(("^Urgrep started.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
     ("^Urgrep finished with \\(?:\\(\\(?:[0-9]+ \\)?match\\(?:es\\)? found\\)\\|\\(no matches found\\)\\).*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 'urgrep-match-count-face nil t)
      (2 'compilation-warning nil t))
     ("^Urgrep \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
      (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
      (1 'compilation-error)
      (2 'compilation-error nil t))))

(defun urgrep--column-begin ()
  "Look forwards for the match highlight to compute the beginning column."
  (let* ((beg (match-end 0))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (mbeg (text-property-any beg end 'font-lock-face 'urgrep-match-face)))
    (when mbeg
      (- mbeg beg))))

(defun urgrep--column-end ()
  "Look forwards for the match highlight to compute the ending column."
  (let* ((beg (match-end 0))
         (end (save-excursion (goto-char beg) (line-end-position)))
         (mbeg (text-property-any beg end 'font-lock-face 'urgrep-match-face))
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
  ;; XXX: Support null separator after filename to make this less brittle, or
  ;; just rely on ANSI escapes as with the match highlight.
  `(;; Ungrouped matches
    (,(concat
       "\\(?1:"
       "\\(?:[a-zA-Z]:\\)?" ; Allow "C:..." for w32.
       "[^\n:]+?[^\n/:]"
       "\\)"
       ;; Use [1-9][0-9]* rather than [0-9]+ to allow ":034:" in file names.
       ":[\t ]*\\(?2:[1-9][0-9]*\\)[\t ]*:")
     1 2 (,#'urgrep--column-begin . ,#'urgrep--column-end))

    ;; Grouped matches
    ("^\\([[:digit:]]+\\):"
     ,#'urgrep--grouped-filename 1
     (,#'urgrep--column-begin . ,#'urgrep--column-end)))
  "Regexp used to match results.
See `compilation-error-regexp-alist' for format details.")

(defun urgrep-command (query)
  (let ((arguments '()))
    (setq arguments (cons (if urgrep-group-matches "--group" "--nogroup")
                          arguments))
    (mapconcat #'shell-quote-argument
               (append '("ag") arguments `(,query))
               " ")))

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
        (while (re-search-forward
                "\033\\[30;43m\\(.*?\\)\033\\[0m\033\\[K" end 1)
          (replace-match
           (propertize (match-string 1) 'face nil 'font-lock-face
                       'urgrep-match-face)
           t t)
          (cl-incf urgrep-num-matches-found))
        ;; Highlight matching filenames and delete ANSI escapes.
        (when urgrep-group-matches
          (goto-char beg)
          (while (re-search-forward
                  "\033\\[1;32m\\(.*\\)\033\\[0m\033\\[K" end 1)
            (replace-match
             (propertize (match-string 1) 'face nil 'font-lock-face
                         'urgrep-hit-face 'urgrep-file-name t)
             t t)))

        ;; Delete all remaining escape sequences.
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[mK]" end 1)
          (replace-match "" t t))))))

(define-compilation-mode urgrep-mode "Urgrep"
  "A compilation mode for various grep-like tools."
  (setq-local compilation-process-setup-function 'urgrep-process-setup
              compilation-error-face 'urgrep-hit-face
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
