;;; urgrep-wgrep.el --- Universal recursive grep -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jim Porter

;; Author: Jim Porter
;; URL: https://github.com/jimporter/urgrep
;; Version: 0.1-dev
;; Keywords:
;; Package-Requires: ((urgrep "0.1-dev"))

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

;; A compatibility layer between urgrep and wgrep.

;;; Code:

(require 'wgrep)

(defvar urgrep-wgrep--grouped-result-regexp
  (concat "^\\(?:"
          ;; A match or context line.
          "\\(?2:[1-9][0-9]*\\)[:=-]"
          "\\|"
          ;; A separator line.
          "\\(?3:--$\\)"
          "\\)")
  "The regexp to use to match results using grouped output.
Group 2 is the line number, and group 3 is the \"--\" separator used when
displaying context.  Group 1 is unused.")

(defvar urgrep-wgrep--ungrouped-result-regexp
  (concat "^\\(?:"
          ;; A match or context line using a null terminator after the filename.
          "\\(?1:[^\0\n]+\\)\0\\(?2:[0-9]+\\)[:=-]"
          "\\|"
          ;; A match or context line without a null terminator; see also
          ;; `urgrep-regexp-alist'.
          "\\(?1:[^\n]*?[^\n/]\\)[:=-]\\(?2:[1-9][0-9]*\\)[:=-]"
          "\\|"
          ;; A separator line.
          "\\(?3:--$\\)"
          "\\)")
    "The regexp to use to match results using ungrouped output.
Group 1 is the filename, group 2 is the line number, and group 3 is the \"--\"
separator used when displaying context.")

(defun urgrep-wgrep--propertize-lines (begin end &optional file-name)
  "Apply wgrep properties to result lines between BEGIN and END.
If END is nil, continue to the end of the (narrowed) buffer.
FILE-NAME, if non-nil is the name of the file for this section of
grouped results; if nil, assume the results are ungrouped."
  (let ((line-regexp (if file-name
                         urgrep-wgrep--grouped-result-regexp
                       urgrep-wgrep--ungrouped-result-regexp)))
    (goto-char begin)
    (while (and (zerop (forward-line 1))
                (looking-at line-regexp)
                (or (not end) (< (point) end)))
      (if (match-beginning 3)
          ;; Ignore the separator line ("--").
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(wgrep-ignore t))
        ;; If results are ungrouped, mark the filename with
        ;; `wgrep-construct-filename-property'.
        (unless file-name
          (let ((this-file (match-string-no-properties 1)))
            (add-text-properties
             (match-beginning 1) (match-end 1)
             (list (wgrep-construct-filename-property file-name) file-name))))
        ;; Mark the line with the filename and line number.
        (add-text-properties
         (match-beginning 0) (match-end 0)
         (list 'wgrep-line-filename file-name
               'wgrep-line-number (string-to-number (match-string 2))))))))

(defun urgrep-wgrep-header/footer-parser ()
  "Mark the header and footer of the urgrep results.
This lets wgrep know what to ignore."
  (save-excursion
    ;; Look for the beginning of the results.
    (goto-char (point-min))
    (if (not (text-property-search-forward 'urgrep-file-name))
        ;; No results, mark the entire buffer as a header.
        (add-text-properties (point-min) (point-max)
                             '(read-only t wgrep-header t))
      ;; Found some results, mark the preceding part of the buffer as a header.
      (add-text-properties (point-min) (line-beginning-position)
                           '(read-only t wgrep-header t))
      ;; Look for the end of the results.
      (goto-char (point-max))
      (text-property-search-backward 'compilation-handle-exit)
      ;; If there's a footer, mark it.
      (unless (eobp)
        (add-text-properties (point) (point-max)
                             '(read-only t wgrep-footer t))))))

(defun urgrep-wgrep-results-parser ()
  "Parse the urgrep results and apply properties for wgrep."
  (save-excursion
    (let ((this-group)
          (next-group (text-property-search-forward 'urgrep-file-name)))
      (if (eolp)
          ;; Grouped results
          (while (prog1 (setq this-group next-group)
                   (setq next-group (text-property-search-forward
                                     'urgrep-file-name)))
            (let* ((file-name-begin (prop-match-beginning this-group))
                   (file-name-end (prop-match-end this-group))
                   (file-name (buffer-substring-no-properties file-name-begin
                                                              file-name-end)))
              ;; Mark the filename with `wgrep-construct-filename-property'.
              (add-text-properties
               file-name-begin file-name-end
               (list 'wgrep-ignore t
                     (wgrep-construct-filename-property file-name) file-name))
              (urgrep-wgrep--propertize-lines
               (prop-match-end this-group)
               (when next-group (prop-match-beginning next-group))
               file-name)))
        ;; Ungrouped results
        (urgrep-wgrep--propertize-lines (point-min) nil)))))

;;;###autoload
(defun urgrep-wgrep-setup ()
  "Set up a urgrep buffer to use wgrep."
  (setq-local wgrep-header/footer-parser #'urgrep-wgrep-header/footer-parser
              wgrep-results-parser #'urgrep-wgrep-results-parser)
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'urgrep-setup-hook #'urgrep-wgrep-setup)

(defun urgrep-wgrep-unload-function ()
  "Unload wgrep from urgrep."
  (remove-hook 'urgrep-setup-hook #'urgrep-wgrep-setup))

(provide 'urgrep-wgrep)
;;; urgrep-wgrep.el ends here
