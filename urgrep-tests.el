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

(defun urgrep-test--check-command (command expected-arguments)
  (should (string= command (mapconcat #'urgrep--maybe-shell-quote-argument
                                      expected-arguments " "))))

(ert-deftest urgrep-tests-command-ripgrep ()
  (let ((tool (assoc "ripgrep" urgrep-tools))
        (common-args '("rg" "--color" "always" "--colors" "path:fg:magenta"
                       "--colors" "match:fg:red" "--colors"
                       "match:style:bold")))
    ;; String/case
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool)
     (append common-args '("--heading" "-i" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool)
     (append common-args '("--heading" "-F" "--" "Foo")))
    (let ((case-fold-search nil))
      (urgrep-test--check-command
       (urgrep-command "foo" :tool tool)
       (append common-args '("--heading" "-F" "--" "foo"))))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold t)
     (append common-args '("--heading" "-i" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold nil)
     (append common-args '("--heading" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold 'smart)
     (append common-args '("--heading" "-i" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool :case-fold 'smart)
     (append common-args '("--heading" "-F" "--" "Foo")))
    ;; Group
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :group nil)
     (append common-args '("--no-heading" "-i" "-F" "--" "foo")))
    ;; Regexp
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp t)
     (append common-args '("--heading" "-i" "--" "\\(foo\\)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'bre)
     (append common-args '("--heading" "-i" "--" "\\(foo\\)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'ere)
     (append common-args '("--heading" "-i" "--" "(foo)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'pcre)
     (append common-args '("--heading" "-i" "--" "(foo)")))
    ;; Context
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context 3)
     (append common-args '("--heading" "-C3" "-i" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(3 . 3))
     (append common-args '("--heading" "-C3" "-i" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(2 . 4))
     (append common-args '("--heading" "-B2" "-A4" "-i" "-F" "--" "foo")))
    ;; File wildcard
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files "*.el")
     (append common-args '("-g" "*.el" "--heading" "-i" "-F" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
     (append common-args '("-g" "*.c" "-g" "*.h" "--heading" "-i" "-F" "--"
                           "foo")))))

(ert-deftest urgrep-tests-command-ag ()
  (let ((tool (assoc "ag" urgrep-tools))
        (common-args '("ag" "--color-path" "35" "--color-match" "1;31")))
    ;; String/case
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool)
     (append common-args '("--group" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool)
     (append common-args '("--group" "-s" "-Q" "--" "Foo")))
    (let ((case-fold-search nil))
      (urgrep-test--check-command
       (urgrep-command "foo" :tool tool)
       (append common-args '("--group" "-s" "-Q" "--" "foo"))))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold t)
     (append common-args '("--group" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold nil)
     (append common-args '("--group" "-s" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold 'smart)
     (append common-args '("--group" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool :case-fold 'smart)
     (append common-args '("--group" "-s" "-Q" "--" "Foo")))
    ;; Group
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :group nil)
     (append common-args '("--nogroup" "-i" "-Q" "--" "foo")))
    ;; Regexp
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp t)
     (append common-args '("--group" "-i" "--" "\\(foo\\)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'bre)
     (append common-args '("--group" "-i" "--" "\\(foo\\)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'ere)
     (append common-args '("--group" "-i" "--" "(foo)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'pcre)
     (append common-args '("--group" "-i" "--" "(foo)")))
    ;; Context
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context 3)
     (append common-args '("--group" "-C3" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(3 . 3))
     (append common-args '("--group" "-C3" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(2 . 4))
     (append common-args '("--group" "-B2" "-A4" "-i" "-Q" "--" "foo")))
    ;; File wildcard
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files "*.el")
     (append common-args '("-G" "^[^\\000]*\\.el$" "--group" "-i" "-Q" "--"
                           "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
     (append common-args '("-G" "^[^\\000]*\\.(c|h)$" "--group" "-i" "-Q" "--"
                           "foo")))))

(ert-deftest urgrep-tests-command-ack ()
  (let ((tool (assoc "ack" urgrep-tools))
        (common-args '("ack" "--color-filename" "magenta" "--color-match"
                       "bold red")))
    ;; String/case
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool)
     (append common-args '("--group" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool)
     (append common-args '("--group" "-Q" "--" "Foo")))
    (let ((case-fold-search nil))
      (urgrep-test--check-command
       (urgrep-command "foo" :tool tool)
       (append common-args '("--group" "-Q" "--" "foo"))))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold t)
     (append common-args '("--group" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold nil)
     (append common-args '("--group" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold 'smart)
     (append common-args '("--group" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool :case-fold 'smart)
     (append common-args '("--group" "-Q" "--" "Foo")))
    ;; Group
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :group nil)
     (append common-args '("--nogroup" "-i" "-Q" "--" "foo")))
    ;; Regexp
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp t)
     (append common-args '("--group" "-i" "--" "\\(foo\\)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'bre)
     (append common-args '("--group" "-i" "--" "\\(foo\\)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'ere)
     (append common-args '("--group" "-i" "--" "(foo)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'pcre)
     (append common-args '("--group" "-i" "--" "(foo)")))
    ;; Context
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context 3)
     (append common-args '("--group" "-C3" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(3 . 3))
     (append common-args '("--group" "-C3" "-i" "-Q" "--" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(2 . 4))
     (append common-args '("--group" "-B2" "-A4" "-i" "-Q" "--" "foo")))
    ;; File wildcard
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files "*.el")
     (append common-args '("-G" "^[^\\000]*\\.el$" "--group" "-i" "-Q" "--"
                           "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
     (append common-args '("-G" "^[^\\000]*\\.(c|h)$" "--group" "-i" "-Q" "--"
                           "foo")))))

(ert-deftest urgrep-tests-command-git-grep ()
  (let ((tool (assoc "git-grep" urgrep-tools))
        (common-args '("git" "--no-pager" "-c" "color.grep.filename=magenta"
                       "-c" "color.grep.match=bold red" "grep" "--color" "-n"
                       "--recurse-submodules"))
        (group-args '("--heading" "--break")))
    ;; String/case
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool)
     (append common-args group-args '("-i" "-F" "-e" "foo" "--")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool)
     (append common-args group-args '("-F" "-e" "Foo" "--")))
    (let ((case-fold-search nil))
      (urgrep-test--check-command
       (urgrep-command "foo" :tool tool)
       (append common-args group-args '("-F" "-e" "foo" "--"))))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold t)
     (append common-args group-args '("-i" "-F" "-e" "foo" "--")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold nil)
     (append common-args group-args '("-F" "-e" "foo" "--")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold 'smart)
     (append common-args group-args '("-i" "-F" "-e" "foo" "--")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool :case-fold 'smart)
     (append common-args group-args '("-F" "-e" "Foo" "--")))
    ;; Group
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :group nil)
     (append common-args '("-i" "-F" "-e" "foo" "--")))
    ;; Regexp
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp t)
     (append common-args group-args '("-i" "-G" "-e" "(foo)" "--")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'bre)
     (append common-args group-args '("-i" "-G" "-e" "(foo)" "--")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'ere)
     (append common-args group-args '("-i" "-E" "-e" "(foo)" "--")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'pcre)
     (append common-args group-args '("-i" "-P" "-e" "(foo)" "--")))
    ;; Context
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context 3)
     (append common-args group-args '("-C3" "-i" "-F" "-e" "foo" "--")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(3 . 3))
     (append common-args group-args '("-C3" "-i" "-F" "-e" "foo" "--")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(2 . 4))
     (append common-args group-args '("-B2" "-A4" "-i" "-F" "-e" "foo" "--")))
    ;; File wildcard
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files "*.el")
     (append common-args group-args '("-i" "-F" "-e" "foo" "--" "*.el")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
     (append common-args group-args '("-i" "-F" "-e" "foo" "--" "*.c" "*.h")))))

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
    (let ((query (shell-quote-argument "(foo)")))
      (should (string-match (concat "^find \\. .*grep -G .*-i .*" query)
                            (urgrep-command "(foo)" :tool tool :regexp t)))
      (should (string-match (concat "^find \\. .*grep -G .*-i .*" query)
                            (urgrep-command "(foo)" :tool tool :regexp 'bre)))
      (should (string-match (concat "^find \\. .*grep -E .*-i .*" query)
                            (urgrep-command "(foo)" :tool tool :regexp 'ere)))
      (should (string-match (concat "^find \\. .*grep -P .*-i .*" query)
                            (urgrep-command "(foo)" :tool tool :regexp 'pcre))))
    ;; Context
    (should (string-match "^find \\. .*grep -F -C3 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context 3)))
    (should (string-match "^find \\. .*grep -F -C3 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context '(3 . 3))))
    (should (string-match "^find \\. .*grep -F -B2 -A4 .*-i .*foo"
                          (urgrep-command "foo" :tool tool :context '(2 . 4))))
    ;; File wildcard
    (let ((escape (lambda (i) (regexp-quote (shell-quote-argument i)))))
      (should (string-match
               (concat "^find \\. .*-name " (funcall escape "*.el")
                       " .*grep -F .*-i .*foo")
               (urgrep-command "foo" :tool tool :files "*.el")))
      (should (string-match
               (concat "^find \\. .*-name " (funcall escape "*.c") " -o -name "
                       (funcall escape "*.h") " .*grep -F .*-i .*foo")
               (urgrep-command "foo" :tool tool :files '("*.c" "*.h")))))))

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
