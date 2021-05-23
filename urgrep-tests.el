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
(unless (fboundp 'always)
  (defun always (&rest _) t))

(ert-deftest urgrep-tests-command-ripgrep ()
  (let ((tool (assoc "ripgrep" urgrep-tools))
        (common-args "rg --color always --colors path\\:fg\\:magenta --colors match\\:fg\\:red --colors match\\:style\\:bold "))
    ;; String/case
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "--heading -i -F -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool)
                   (concat common-args "--heading -F -- Foo")))
    (let ((case-fold-search nil))
      (should (equal (urgrep-command "foo" :tool tool)
                     (concat common-args "--heading -F -- foo"))))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "--no-heading -i -F -- foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'bre)
                   (concat common-args "--heading -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'ere)
                   (concat common-args "--heading -i -- \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'pcre)
                   (concat common-args "--heading -i -- \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--heading -C3 -i -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--heading -C3 -i -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--heading -B2 -A4 -i -F -- foo")))))

(ert-deftest urgrep-tests-command-ag ()
  (let ((tool (assoc "ag" urgrep-tools))
        (common-args "ag --color-path 35 --color-match 1\\;31 "))
    ;; String/case
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "--group -i -Q -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool)
                   (concat common-args "--group -s -Q -- Foo")))
    (let ((case-fold-search nil))
      (should (equal (urgrep-command "foo" :tool tool)
                     (concat common-args "--group -s -Q -- foo"))))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "--nogroup -i -Q -- foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'bre)
                   (concat common-args "--group -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'ere)
                   (concat common-args "--group -i -- \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'pcre)
                   (concat common-args "--group -i -- \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--group -B2 -A4 -i -Q -- foo")))))

(ert-deftest urgrep-tests-command-ack ()
  (let ((tool (assoc "ack" urgrep-tools))
        (common-args "ack --color-filename magenta --color-match bold\\ red "))
    ;; String/case
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "--group -i -Q -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool)
                   (concat common-args "--group -Q -- Foo")))
    (let ((case-fold-search nil))
      (should (equal (urgrep-command "foo" :tool tool)
                     (concat common-args "--group -Q -- foo"))))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "--nogroup -i -Q -- foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'bre)
                   (concat common-args "--group -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'ere)
                   (concat common-args "--group -i -- \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'pcre)
                   (concat common-args "--group -i -- \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--group -B2 -A4 -i -Q -- foo")))))

(ert-deftest urgrep-tests-command-git-grep ()
  (let ((tool (assoc "git-grep" urgrep-tools))
        (common-args "git --no-pager -c color.grep.filename\\=magenta -c color.grep.match\\=bold\\ red grep --color -n --recurse-submodules "))
    ;; String/case
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "--heading --break -i -F -e foo")))
    (should (equal (urgrep-command "Foo" :tool tool)
                   (concat common-args "--heading --break -F -e Foo")))
    (let ((case-fold-search nil))
      (should (equal (urgrep-command "foo" :tool tool)
                     (concat common-args "--heading --break -F -e foo"))))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "-i -F -e foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'bre)
                   (concat common-args "--heading --break -i -G -e \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'ere)
                   (concat common-args "--heading --break -i -E -e \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp-syntax 'pcre)
                   (concat common-args "--heading --break -i -P -e \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--heading --break -C3 -i -F -e foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--heading --break -C3 -i -F -e foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--heading --break -B2 -A4 -i -F -e foo")))))

(ert-deftest urgrep-tests-command-grep ()
  (let ((tool (assoc "grep" urgrep-tools)))
    ;; String/case
    (should (string-match "^find \\. .*grep -F .*-i .*foo"
                          (urgrep-command "foo" :tool tool)))
    (should (string-match "^find \\. .*grep -F .*Foo"
                          (urgrep-command "Foo" :tool tool)))
    (let ((case-fold-search nil))
      (should (string-match "^find \\. .*grep -F .*foo"
                            (urgrep-command "foo" :tool tool))))
    ;; Group
    (should (string-match "^find \\. .*grep -F .*-i .*foo"
                          (urgrep-command "foo" :tool tool :group nil)))
    ;; Regexp
    (should (string-match "^find \\. .*grep -G .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool
                                          :regexp-syntax 'bre)))
    (should (string-match "^find \\. .*grep -E .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool
                                          :regexp-syntax 'ere)))
    (should (string-match "^find \\. .*grep -P .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool
                                          :regexp-syntax 'pcre)))
    ;; Context
    (should (string-match "^find \\. .*grep -F -C3 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context 3)))
    (should (string-match "^find \\. .*grep -F -C3 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context '(3 . 3))))
    (should (string-match "^find \\. .*grep -F -B2 -A4 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context '(2 . 4))))))

(ert-deftest urgrep-tests-get-tool-default ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults '())
           (tool (urgrep-get-tool)))
      (should (equal (car tool) "ripgrep"))
      (should (equal (urgrep-get-property tool 'executable-name) "rg"))
      (should (equal urgrep--host-defaults '((localhost . "ripgrep")))))))

(ert-deftest urgrep-tests-get-tool-default-cached ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults '((localhost . "ag")))
           (tool (urgrep-get-tool)))
      (should (equal (car tool) "ag"))
      (should (equal (urgrep-get-property tool 'executable-name) "ag"))
      (should (equal urgrep--host-defaults '((localhost . "ag")))))))

(ert-deftest urgrep-tests-get-tool-string ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults '())
           (tool (urgrep-get-tool "ag")))
      (should (equal (car tool) "ag"))
      (should (equal (urgrep-get-property tool 'executable-name) "ag"))
      (should (equal urgrep--host-defaults '())))))

(ert-deftest urgrep-tests-get-tool-cons ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults '())
           (tool (urgrep-get-tool '("goofy" (executable-name "gf")))))
      (should (equal (car tool) "goofy"))
      (should (equal (urgrep-get-property tool 'executable-name) "gf"))
      (should (equal urgrep--host-defaults '())))))

(defun urgrep-tests--check-match-at-point ()
  (let* ((line (string-to-number (current-word)))
         (loc
          (compilation--message->loc
           (get-text-property (point) 'compilation-message)))
         (text-start (re-search-forward ":"))
         (text-end (line-end-position))
         (match-start (text-property-any text-start text-end 'font-lock-face
                                         'urgrep-match)))
    (should (equal (caar (compilation--loc->file-struct loc))
                   "urgrep-tests.el"))
    (should (equal (compilation--loc->line loc) line))
    (should (equal (compilation--loc->col loc)
                   (- match-start text-start)))))

(ert-deftest urgrep-tests-urgrep-group ()
  (switch-to-buffer (urgrep "urgrep" nil))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests--check-match-at-point))

(ert-deftest urgrep-tests-urgrep-nogroup ()
  (switch-to-buffer (urgrep "urgrep" nil :group nil))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el:")
  (urgrep-tests--check-match-at-point))

;;; urgrep-tests.el ends here
