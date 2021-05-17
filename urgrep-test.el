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
  (let ((tool (assoc "ag" urgrep-tools))
        (common-args "ag --color-path 35 --color-match 1\\;31 "))
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "-Q --group foo")))
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "-Q --nogroup foo")))
    (should (equal (urgrep-command "foo" :tool tool :regexp t)
                   (concat common-args "--group foo")))
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "-C3 -Q --group foo")))))

(ert-deftest urgrep-test-command-git-grep ()
  (let ((tool (assoc "git-grep" urgrep-tools))
        (common-args "git -c color.grep.filename\\=magenta grep -n --recurse-submodules --color "))
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "-F --heading --break foo")))
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "-F foo")))
    (should (equal (urgrep-command "foo" :tool tool :regexp t)
                   (concat common-args "--heading --break foo")))
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "-C3 -F --heading --break foo")))))

(ert-deftest urgrep-test-command-grep ()
  (let ((tool (assoc "grep" urgrep-tools)))
    (should (string-match "^find \\."
                          (urgrep-command "foo" :tool tool)))
    (should (string-match "^find \\."
                          (urgrep-command "foo" :tool tool :group nil)))
    (should (string-match "^find \\."
                          (urgrep-command "foo" :tool tool :regexp t)))
    (should (string-match "^find \\."
                          (urgrep-command "foo" :tool tool :context 3)))))

(defun urgrep-test--check-match-at-point ()
  (let* ((line (string-to-number (current-word)))
         (loc
          (compilation--message->loc
           (get-text-property (point) 'compilation-message)))
         (text-start (re-search-forward ":"))
         (text-end (line-end-position))
         (match-start (text-property-any text-start text-end 'font-lock-face
                                         'urgrep-match)))
    (should (equal (caar (compilation--loc->file-struct loc))
                   "urgrep-test.el"))
    (should (equal (compilation--loc->line loc) line))
    (should (equal (compilation--loc->col loc)
                   (- match-start text-start)))))

(ert-deftest urgrep-test-urgrep-group ()
  (switch-to-buffer (urgrep "urgrep" nil))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-test.el")
  (beginning-of-line 2)
  (urgrep-test--check-match-at-point))

(ert-deftest urgrep-test-urgrep-nogroup ()
  (switch-to-buffer (urgrep "urgrep" nil :group nil))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-test.el:")
  (urgrep-test--check-match-at-point))

;;; urgrep-test.el ends here
