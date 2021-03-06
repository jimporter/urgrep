;;; urgrep-tests.el --- Tests for urgrep -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Jim Porter

;; Author: Jim Porter
;; Keywords: tests

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

(ert-deftest urgrep-tests-command-ugrep ()
  (let ((tool (assq 'ugrep urgrep-tools))
        (common-args '("ugrep" "--color=always"
                       "--colors=mt=01;31:fn=35:ln=:bn=:se=:sl=:cx=:ne"
                       "-n" "--ignore-files")))
    ;; String/case
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool)
     (append common-args '("--heading" "--break" "-i" "-F" "-e" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool)
     (append common-args '("--heading" "--break" "-F" "-e" "Foo")))
    (let ((case-fold-search nil))
      (urgrep-test--check-command
       (urgrep-command "foo" :tool tool)
       (append common-args '("--heading" "--break" "-F" "-e" "foo"))))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold t)
     (append common-args '("--heading" "--break" "-i" "-F" "-e" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold nil)
     (append common-args '("--heading" "--break" "-F" "-e" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :case-fold 'smart)
     (append common-args '("--heading" "--break" "-i" "-F" "-e" "foo")))
    (urgrep-test--check-command
     (urgrep-command "Foo" :tool tool :case-fold 'smart)
     (append common-args '("--heading" "--break" "-F" "-e" "Foo")))
    ;; Group
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :group nil)
     (append common-args '("-i" "-F" "-e" "foo")))
    ;; Regexp
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp t)
     (append common-args '("--heading" "--break" "-i" "-G" "-e" "(foo)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'bre)
     (append common-args '("--heading" "--break" "-i" "-G" "-e" "(foo)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'ere)
     (append common-args '("--heading" "--break" "-i" "-E" "-e" "(foo)")))
    (urgrep-test--check-command
     (urgrep-command "(foo)" :tool tool :regexp 'pcre)
     (append common-args '("--heading" "--break" "-i" "-P" "-e" "(foo)")))
    ;; Context
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context 3)
     (append common-args '("--heading" "--break" "-C3" "-i" "-F" "-e" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(3 . 3))
     (append common-args '("--heading" "--break" "-C3" "-i" "-F" "-e" "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :context '(2 . 4))
     (append common-args '("--heading" "--break" "-B2" "-A4" "-i" "-F" "-e"
                           "foo")))
    ;; File wildcard
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files "*.el")
     (append common-args '("--include=*.el" "--heading" "--break" "-i" "-F" "-e"
                           "foo")))
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :files '("*.c" "*.h"))
     (append common-args '("--include=*.c" "--include=*.h" "--heading" "--break"
                           "-i" "-F" "-e" "foo")))
    ;; Color
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :color nil)
     (append '("ugrep" "--color=never" "-n" "--ignore-files" "--heading"
               "--break" "-i" "-F" "-e" "foo")))))

(ert-deftest urgrep-tests-command-ripgrep ()
  (let ((tool (assq 'ripgrep urgrep-tools))
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
                           "foo")))
    ;; Color
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :color nil)
     (append '("rg" "--color" "never" "--heading" "-i" "-F" "--" "foo")))))

(ert-deftest urgrep-tests-command-ag ()
  (let ((tool (assq 'ag urgrep-tools))
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
                           "foo")))
    ;; Color
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :color nil)
     (append '("ag" "--nocolor" "--group" "-i" "-Q" "--" "foo")))))

(ert-deftest urgrep-tests-command-ack ()
  (let ((tool (assq 'ack urgrep-tools))
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
                           "foo")))
    ;; Color
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :color nil)
     (append '("ack" "--nocolor" "--group" "-i" "-Q" "--" "foo")))))

(ert-deftest urgrep-tests-command-git-grep ()
  (let ((tool (assq 'git-grep urgrep-tools))
        (common-args '("git" "--no-pager" "-c" "color.grep.filename=magenta"
                       "-c" "color.grep.match=bold red" "grep" "--color"
                       "--no-index" "--exclude-standard" "-n"))
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
     (append common-args group-args '("-i" "-F" "-e" "foo" "--" "*.c" "*.h")))
    ;; Color
    (urgrep-test--check-command
     (urgrep-command "foo" :tool tool :color nil)
     (append
      '("git" "--no-pager" "grep" "--no-color" "--no-index"
        "--exclude-standard" "-n")
      group-args
      '("-i" "-F" "-e" "foo" "--")))))

(ert-deftest urgrep-tests-command-grep ()
  (let ((tool (assq 'grep urgrep-tools))
        (template (concat "^find \\(\\|.+ \\)\\. \\(\\|.+ \\)%s\\(\\|.+ \\)"
                          "grep %s\\(\\|.+ \\)%s"))
        (escape (lambda (i) (regexp-quote (shell-quote-argument i)))))
    ;; String/case
    (should (string-match (format template "" "--color=always -i -F" "foo")
                          (urgrep-command "foo" :tool tool)))
    (should (string-match (format template "" "--color=always -F" "Foo")
                          (urgrep-command "Foo" :tool tool)))
    (let ((case-fold-search nil))
      (should (string-match (format template "" "--color=always -F" "foo")
                            (urgrep-command "foo" :tool tool))))
    (should (string-match (format template "" "--color=always -i -F" "foo")
                          (urgrep-command "foo" :tool tool :case-fold t)))
    (should (string-match (format template "" "--color=always -F" "foo")
                          (urgrep-command "foo" :tool tool :case-fold nil)))
    (should (string-match (format template "" "--color=always -i -F" "foo")
                          (urgrep-command "foo" :tool tool :case-fold 'smart)))
    (should (string-match (format template "" "--color=always -F" "Foo")
                          (urgrep-command "Foo" :tool tool :case-fold 'smart)))
    ;; Group
    (should (string-match (format template "" "--color=always -i -F" "foo")
                          (urgrep-command "foo" :tool tool :group nil)))
    ;; Regexp
    (let ((query (funcall escape "(foo)")))
      (should (string-match (format template "" "--color=always -i -G" query)
                            (urgrep-command "(foo)" :tool tool :regexp t)))
      (should (string-match (format template "" "--color=always -i -G" query)
                            (urgrep-command "(foo)" :tool tool :regexp 'bre)))
      (should (string-match (format template "" "--color=always -i -E" query)
                            (urgrep-command "(foo)" :tool tool :regexp 'ere)))
      (should (string-match (format template "" "--color=always -i -P" query)
                            (urgrep-command "(foo)" :tool tool :regexp 'pcre))))
    ;; Context
    (should (string-match (format template "" "--color=always -C3 -i -F" "foo")
                          (urgrep-command "foo" :tool tool :context 3)))
    (should (string-match (format template "" "--color=always -C3 -i -F" "foo")
                          (urgrep-command "foo" :tool tool :context '(3 . 3))))
    (should (string-match (format template "" "--color=always -B2 -A4 -i -F"
                                  "foo")
                          (urgrep-command "foo" :tool tool :context '(2 . 4))))
    ;; File wildcard
    (let ((escape (lambda (i) (regexp-quote (shell-quote-argument i)))))
      (should (string-match
               (format template (concat "-i?name " (funcall escape "*.el") " ")
                       "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :files "*.el")))
      (should (string-match
               (format template (concat "-i?name " (funcall escape "*.c") " -o "
                                        "-i?name " (funcall escape "*.h") " ")
                       "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :files '("*.c" "*.h")))))
    ;; Color
    (should (string-match (format template "" "+-i -F" "foo")
                          (urgrep-command "foo" :tool tool :color nil)))))

(ert-deftest urgrep-tests-get-tool-default ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults)
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ugrep))
      (should (equal (urgrep--get-prop 'executable-name tool) "ugrep"))
      (should (equal urgrep--host-defaults `((localhost . ,tool)))))))

(ert-deftest urgrep-tests-get-tool-default-cached ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((ag (assq 'ag urgrep-tools))
           (urgrep--host-defaults `((localhost . ,ag)))
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "ag"))
      (should (equal urgrep--host-defaults `((localhost . ,ag)))))))

(ert-deftest urgrep-tests-get-tool-preferred ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults)
           (urgrep-preferred-tools '(ag grep))
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "ag"))
      (should (equal urgrep--host-defaults `((localhost . ,tool)))))))

(ert-deftest urgrep-tests-get-tool-preferred-cons ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults)
           (urgrep-preferred-tools '((ag . "/usr/bin/ag")))
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "/usr/bin/ag"))
      (should (equal urgrep--host-defaults `((localhost . ,tool)))))))

(ert-deftest urgrep-tests-get-tool-key ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults)
           (tool (urgrep-get-tool 'ag)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "ag"))
      (should (equal urgrep--host-defaults nil)))))

(ert-deftest urgrep-tests-get-tool-cons ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--host-defaults)
           (tool (urgrep-get-tool '(goofy (executable-name . "gf")))))
      (should (equal (car tool) 'goofy))
      (should (equal (urgrep--get-prop 'executable-name tool) "gf"))
      (should (equal urgrep--host-defaults nil)))))

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
  (switch-to-buffer (urgrep "urgrep"))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query '("urgrep"))
               (local-variable-p 'urgrep-current-query)))
  (while (get-buffer-process (current-buffer))
    (sit-for 0.01))
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests--check-match-at-point))

(ert-deftest urgrep-tests-urgrep-nogroup ()
  (switch-to-buffer (urgrep "urgrep" :group nil))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query '("urgrep" :group nil))
               (local-variable-p 'urgrep-current-query)))
  (while (get-buffer-process (current-buffer))
    (sit-for 0.01))
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el:")
  (urgrep-tests--check-match-at-point))

(ert-deftest urgrep-tests-urgrep-run-command ()
  (switch-to-buffer (urgrep-run-command (urgrep-command "urgrep") nil nil))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query (urgrep-command "urgrep"))
               (local-variable-p 'urgrep-current-query)))
  (while (get-buffer-process (current-buffer))
    (sit-for 0.01))
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests--check-match-at-point))

;;; urgrep-tests.el ends here
