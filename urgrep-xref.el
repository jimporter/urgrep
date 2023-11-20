;;; urgrep-xref.el --- Universal recursive grep -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Jim Porter
;; URL: https://github.com/jimporter/urgrep
;; Version: 0.3.0-git
;; Keywords: grep, search

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

;; A compatibility layer between urgrep and xref.

;;; Code:

(require 'urgrep)

(defalias 'urgrep--remove-directories
  (if (>= emacs-major-version 30)
      #'identity
    (lambda (files)
      "Work around Emacs bug#66806."
      (seq-filter (lambda (i) (not (file-directory-p i))) files))))

(defun urgrep--xref-matches-in-files (oldfun regexp files)
  "Override `xref-matches-in-files' to use `urgrep-command'."
  (cl-letf* ((xargs-max-chars
              (and (memq system-type '(windows-nt ms-dos))
                   "-s 10000 "))
             (old-grep-expand-template (symbol-function 'grep-expand-template))
             ((symbol-function 'grep-expand-template)
              (lambda (_template regexp)
                (setf (symbol-function 'grep-expand-template)
                      old-grep-expand-template)
                (concat "xargs -0 " xargs-max-chars
                        (urgrep-command regexp :regexp 'ere :group nil
                                        :color nil :root nil)))))
    (funcall oldfun regexp (urgrep--remove-directories files))))

(advice-add #'xref-matches-in-files :around #'urgrep--xref-matches-in-files)

(defun urgrep-xref-unload-function ()
  (advice-remove #'xref-matches-in-files #'urgrep--xref-matches-in-files))

(provide 'urgrep-xref)
;;; urgrep-xref.el ends here
