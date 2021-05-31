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

(ert-deftest urgrep-tests-common-prefix ()
  (should (equal (urgrep--common-prefix "foo" "bar") ""))
  (should (equal (urgrep--common-prefix "bar" "baz") "ba")))

(ert-deftest urgrep-tests-wildcards-to-regexp ()
  (should (equal (urgrep--wildcards-to-regexp nil 'pcre) "^$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.el") 'pcre)
                 "^[^\\000]*\\.el$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.cpp" "*.hpp") 'pcre)
                 "^[^\\000]*\\.(cpp|hpp)$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.cpp" "*.c") 'pcre)
                 "^[^\\000]*\\.c(pp|)$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.[ab]cpp" "*.[ab]c") 'pcre)
                 "^[^\\000]*\\.([ab]cpp|[ab]c)$")))

(ert-deftest urgrep-tests-command-ripgrep ()
  (let ((tool (assoc "ripgrep" urgrep-tools))
        (common-args (concat "rg --color always --colors path\\:fg\\:magenta "
                             "--colors match\\:fg\\:red "
                             "--colors match\\:style\\:bold ")))
    ;; String/case
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args "--heading -i -F -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool)
                   (concat common-args "--heading -F -- Foo")))
    (let ((case-fold-search nil))
      (should (equal (urgrep-command "foo" :tool tool)
                     (concat common-args "--heading -F -- foo"))))
    (should (equal (urgrep-command "foo" :tool tool :case-fold t)
                   (concat common-args "--heading -i -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold nil)
                   (concat common-args "--heading -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold 'smart)
                   (concat common-args "--heading -i -F -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool :case-fold 'smart)
                   (concat common-args "--heading -F -- Foo")))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "--no-heading -i -F -- foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'bre)
                   (concat common-args "--heading -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'ere)
                   (concat common-args "--heading -i -- \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'pcre)
                   (concat common-args "--heading -i -- \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--heading -C3 -i -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--heading -C3 -i -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--heading -B2 -A4 -i -F -- foo")))
    ;; File wildcard
    (should (equal (urgrep-command "foo" :tool tool :files "*.el")
                   (concat common-args "-g \\*.el --heading -i -F -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
                   (concat common-args "-g \\*.c -g \\*.h --heading -i -F -- "
                           "foo")))))

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
    (should (equal (urgrep-command "foo" :tool tool :case-fold t)
                   (concat common-args "--group -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold nil)
                   (concat common-args "--group -s -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold 'smart)
                   (concat common-args "--group -i -Q -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool :case-fold 'smart)
                   (concat common-args "--group -s -Q -- Foo")))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "--nogroup -i -Q -- foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp t)
                   (concat common-args "--group -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'bre)
                   (concat common-args "--group -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'ere)
                   (concat common-args "--group -i -- \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'pcre)
                   (concat common-args "--group -i -- \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--group -B2 -A4 -i -Q -- foo")))
    ;; File wildcard
    (should (equal (urgrep-command "foo" :tool tool :files "*.el")
                   (concat common-args "-G \\^\\[\\^\\\\000\\]\\*\\\\.el\\$ "
                           "--group -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
                   (concat common-args
                           "-G \\^\\[\\^\\\\000\\]\\*\\\\.\\(c\\|h\\)\\$ "
                           "--group -i -Q -- foo")))))

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
    (should (equal (urgrep-command "foo" :tool tool :case-fold t)
                   (concat common-args "--group -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold nil)
                   (concat common-args "--group -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold 'smart)
                   (concat common-args "--group -i -Q -- foo")))
    (should (equal (urgrep-command "Foo" :tool tool :case-fold 'smart)
                   (concat common-args "--group -Q -- Foo")))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "--nogroup -i -Q -- foo")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp t)
                   (concat common-args "--group -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'bre)
                   (concat common-args "--group -i -- \\\\\\(foo\\\\\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'ere)
                   (concat common-args "--group -i -- \\(foo\\)")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'pcre)
                   (concat common-args "--group -i -- \\(foo\\)")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args "--group -C3 -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args "--group -B2 -A4 -i -Q -- foo")))
    ;; File wildcard
    (should (equal (urgrep-command "foo" :tool tool :files "*.el")
                   (concat common-args "-G \\^\\[\\^\\\\000\\]\\*\\\\.el\\$ "
                           "--group -i -Q -- foo")))
    (should (equal (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
                   (concat common-args
                           "-G \\^\\[\\^\\\\000\\]\\*\\\\.\\(c\\|h\\)\\$ "
                           "--group -i -Q -- foo")))))

(ert-deftest urgrep-tests-command-git-grep ()
  (let ((tool (assoc "git-grep" urgrep-tools))
        (common-args (concat "git --no-pager -c color.grep.filename\\=magenta "
                             "-c color.grep.match\\=bold\\ red grep --color -n "
                             "--recurse-submodules "))
        (group-args "--heading --break "))
    ;; String/case
    (should (equal (urgrep-command "foo" :tool tool)
                   (concat common-args group-args "-i -F -e foo --")))
    (should (equal (urgrep-command "Foo" :tool tool)
                   (concat common-args group-args "-F -e Foo --")))
    (let ((case-fold-search nil))
      (should (equal (urgrep-command "foo" :tool tool)
                     (concat common-args group-args "-F -e foo --"))))
    (should (equal (urgrep-command "foo" :tool tool :case-fold t)
                   (concat common-args group-args "-i -F -e foo --")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold nil)
                   (concat common-args group-args "-F -e foo --")))
    (should (equal (urgrep-command "foo" :tool tool :case-fold 'smart)
                   (concat common-args group-args "-i -F -e foo --")))
    (should (equal (urgrep-command "Foo" :tool tool :case-fold 'smart)
                   (concat common-args group-args "-F -e Foo --")))
    ;; Group
    (should (equal (urgrep-command "foo" :tool tool :group nil)
                   (concat common-args "-i -F -e foo --")))
    ;; Regexp
    (should (equal (urgrep-command "(foo)" :tool tool :regexp t)
                   (concat common-args group-args "-i -G -e \\(foo\\) --")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'bre)
                   (concat common-args group-args "-i -G -e \\(foo\\) --")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'ere)
                   (concat common-args group-args "-i -E -e \\(foo\\) --")))
    (should (equal (urgrep-command "(foo)" :tool tool :regexp 'pcre)
                   (concat common-args group-args "-i -P -e \\(foo\\) --")))
    ;; Context
    (should (equal (urgrep-command "foo" :tool tool :context 3)
                   (concat common-args group-args "-C3 -i -F -e foo --")))
    (should (equal (urgrep-command "foo" :tool tool :context '(3 . 3))
                   (concat common-args group-args "-C3 -i -F -e foo --")))
    (should (equal (urgrep-command "foo" :tool tool :context '(2 . 4))
                   (concat common-args group-args "-B2 -A4 -i -F -e foo --")))
    ;; File wildcard
    (should (equal (urgrep-command "foo" :tool tool :files "*.el")
                   (concat common-args group-args "-i -F -e foo -- \\*.el")))
    (should (equal (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
                   (concat common-args group-args "-i -F -e foo -- \\*.c "
                           "\\*.h")))))

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
    (should (string-match "^find \\. .*grep -F .*-i .*foo"
                          (urgrep-command "foo" :tool tool :case-fold t)))
    (should (string-match "^find \\. .*grep -F .*foo"
                          (urgrep-command "foo" :tool tool :case-fold nil)))
    (should (string-match "^find \\. .*grep -F .*-i .*foo"
                          (urgrep-command "foo" :tool tool :case-fold 'smart)))
    (should (string-match "^find \\. .*grep -F .*Foo"
                          (urgrep-command "Foo" :tool tool :case-fold 'smart)))
    ;; Group
    (should (string-match "^find \\. .*grep -F .*-i .*foo"
                          (urgrep-command "foo" :tool tool :group nil)))
    ;; Regexp
    (should (string-match "^find \\. .*grep -G .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool :regexp t)))
    (should (string-match "^find \\. .*grep -G .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool :regexp 'bre)))
    (should (string-match "^find \\. .*grep -E .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool :regexp 'ere)))
    (should (string-match "^find \\. .*grep -P .*-i .*\\\\(foo\\\\)"
                          (urgrep-command "(foo)" :tool tool :regexp 'pcre)))
    ;; Context
    (should (string-match "^find \\. .*grep -F -C3 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context 3)))
    (should (string-match "^find \\. .*grep -F -C3 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context '(3 . 3))))
    (should (string-match "^find \\. .*grep -F -B2 -A4 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context '(2 . 4))))
    ;; File wildcard
    (should (string-match "^find \\. .*-name \\\\\\*\\.el .*grep -F .*-i .*foo"
                          (urgrep-command "foo" :tool tool :files "*.el")))
    (should (string-match (concat "^find \\. .*-name \\\\\\*\\.c -o "
                                  "-name \\\\\\*\\.h .*grep -F .*-i .*foo")
                          (urgrep-command "foo" :tool tool
                                          :files '("*.c" "*.h"))))))

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
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query '("urgrep"))
               (local-variable-p 'urgrep-current-query)))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests--check-match-at-point))

(ert-deftest urgrep-tests-urgrep-nogroup ()
  (switch-to-buffer (urgrep "urgrep" nil :group nil))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query '("urgrep" :group nil))
               (local-variable-p 'urgrep-current-query)))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el:")
  (urgrep-tests--check-match-at-point))

(ert-deftest urgrep-tests-urgrep-run-command ()
  (switch-to-buffer (urgrep-run-command (urgrep-command "urgrep") nil nil))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query (urgrep-command "urgrep"))
               (local-variable-p 'urgrep-current-query)))
  (sit-for 1)
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests--check-match-at-point))

;;; urgrep-tests.el ends here
