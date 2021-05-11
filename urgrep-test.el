;;; urgrep-test.el --- Tests for urgrep -*- lexical-binding: t -*-

;; Copyright (C) 2021 Jim Porter

;; Author: Jim Porter
;; Keywords: tests

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

;; Tests for urgrep

;;; Code:

(require 'ert)

(ert-deftest urgrep-test-command-ag ()
  (cl-letf (((symbol-function #'urgrep-get-tool)
             (lambda () (assoc "ag" urgrep-tools))))
    (should (equal (urgrep-command "foo")
                   "ag --group --color-match 1\\;31 foo"))
    (let ((urgrep-group-matches nil))
      (should (equal (urgrep-command "foo")
                     "ag --nogroup --color-match 1\\;31 foo")))))

(ert-deftest urgrep-test-command-grep ()
  (cl-letf (((symbol-function #'urgrep-get-tool)
             (lambda () (assoc "grep" urgrep-tools))))
    (should (string-match "^find \\." (urgrep-command "foo")))
    (let ((urgrep-group-matches nil))
      (should (string-match "^find \\." (urgrep-command "foo"))))))

(defun urgrep-test--check-match-at-point ()
  (let* ((line (string-to-number (current-word)))
         (loc
          (compilation--message->loc
           (get-text-property (point) 'compilation-message)))
         (text-start (re-search-forward ":"))
         (text-end (line-end-position))
         (match-start (text-property-any text-start text-end 'font-lock-face
                                         'urgrep-match-face)))
    (should (equal (caar (compilation--loc->file-struct loc))
                   "urgrep-test.el"))
    (should (equal (compilation--loc->line loc) line))
    (should (equal (compilation--loc->col loc)
                   (- match-start text-start)))))

(ert-deftest urgrep-test-urgrep-group ()
  (switch-to-buffer (urgrep "urgrep"))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-test.el")
  (beginning-of-line 2)
  (urgrep-test--check-match-at-point))

(ert-deftest urgrep-test-urgrep-nogroup ()
  (let ((urgrep-group-matches nil))
    (switch-to-buffer (urgrep "urgrep")))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-test.el:")
  (urgrep-test--check-match-at-point))

;;; urgrep-test.el ends here
