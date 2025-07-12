;;; urgrep-tests.el --- Tests for urgrep -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Jim Porter
;; URL: https://github.com/jimporter/urgrep
;; Keywords: tests

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

;; Tests for urgrep

;;; Code:

(require 'compat)
(require 'ert)
(require 'eshell)
(require 'esh-mode)
(require 'tramp)
(require 'urgrep)

;; Work around Emacs bug#58265.
(let ((orig-home (getenv "HOME")))
  (require 'ert-x)
  (when (< emacs-major-version 29)
    (setenv "HOME" orig-home)))
(eval-when-compile (require 'ert-x))

(defun urgrep-tests/remote-accessible-p ()
  "Return whether a test involving remote files can proceed."
  (let ((inhibit-message t))
    (ignore-errors
      (and
       (boundp 'ert-remote-temporary-file-directory)
       (file-remote-p ert-remote-temporary-file-directory)
       (file-directory-p ert-remote-temporary-file-directory)
       (file-writable-p ert-remote-temporary-file-directory)))))

(defun urgrep-tests/check-command (command expected-arguments)
  "Ensure that COMMAND is equivalent to EXPECTED-ARGUMENTS.
EXPECTED-ARGUMENTS should be a list, which will be quoted and
joined to compare against COMMAND."
  (should (string= command (mapconcat #'urgrep--maybe-shell-quote-argument
                                      expected-arguments " "))))

(defun urgrep-tests/check-match-at-point (&optional filename)
  "In a Urgrep buffer, check that the match at point is consistent."
  (let* ((line (string-to-number (current-word)))
         (loc (compilation--message->loc
               (get-text-property (point) 'compilation-message)))
         (text-start (re-search-forward ":"))
         (text-end (line-end-position))
         (match-start (text-property-any text-start text-end 'font-lock-face
                                         'urgrep-match)))
    (should (equal (caar (compilation--loc->file-struct loc))
                   (or filename "urgrep-tests.el")))
    (should (equal (compilation--loc->line loc) line))
    (should (equal (compilation--loc->col loc)
                   (- match-start text-start)))))

;; Eshell utilities:

;; These are adapted from test/lisp/eshell/eshell-tests-helpers.el in Emacs.

(defvar eshell-history-file-name)
(defvar eshell-last-dir-ring-file-name)

(defmacro with-temp-eshell (&rest body)
  "Evaluate BODY in a temporary Eshell buffer."
  `(save-current-buffer
     (ert-with-temp-directory eshell-directory-name
       (let* (;; We want no history file, so prevent Eshell from falling
              ;; back on $HISTFILE.
              (process-environment (cons "HISTFILE" process-environment))
              (eshell-history-file-name nil)
              (eshell-last-dir-ring-file-name nil)
              (eshell-buffer (eshell t)))
         (unwind-protect
             (with-current-buffer eshell-buffer
               ,@body)
           (let (kill-buffer-query-functions)
             (kill-buffer eshell-buffer)))))))

(defun eshell-match-command-output (command regexp)
  "Insert a COMMAND at the end of the buffer and match the output with REGEXP."
  ;; Execute COMMAND.
  (goto-char eshell-last-output-end)
  (insert-and-inherit command)
  (eshell-send-input)
  ;; Wait until the command has completed.
  (let ((start (current-time)))
    (while (eshell-interactive-process)
      (when (> (float-time (time-since start)) 5)
        (error "timeout"))
      (accept-process-output)))
  ;; Check the result.
  (should (string-match-p regexp
                          (buffer-substring-no-properties
                           (eshell-beginning-of-output)
                           (eshell-end-of-output)))))

;;; Tests:

(ert-deftest urgrep-tests/common-prefix ()
  (should (equal (urgrep--common-prefix "foo" "bar") ""))
  (should (equal (urgrep--common-prefix "bar" "baz") "ba")))

(ert-deftest urgrep-tests/wildcards-to-regexp ()
  (should (equal (urgrep--wildcards-to-regexp nil 'pcre) "^$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.el") 'pcre)
                 "^[^\\000]*\\.el$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.cpp" "*.hpp") 'pcre)
                 "^[^\\000]*\\.(cpp|hpp)$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.cpp" "*.c") 'pcre)
                 "^[^\\000]*\\.c(pp|)$"))
  (should (equal (urgrep--wildcards-to-regexp '("*.[ab]cpp" "*.[ab]c") 'pcre)
                 "^[^\\000]*\\.([ab]cpp|[ab]c)$")))

(ert-deftest urgrep-tests/command/ugrep ()
  (let ((tool (assq 'ugrep urgrep-tools))
        (common-args '("ugrep" "--color=always"
                       "--colors=mt=01;31:fn=35:ln=:bn=:se=:sl=:cx=:ne"
                       "-rn" "--ignore-files")))
    (ert-info ("String/case")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool)
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool)
       `(,@common-args "--heading" "--break" "-F" "-e" "Foo" "."))
      (let ((case-fold-search nil))
        (urgrep-tests/check-command
         (urgrep-command "foo" :tool tool)
         `(,@common-args "--heading" "--break" "-F" "-e" "foo" ".")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold t)
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold nil)
       `(,@common-args "--heading" "--break" "-F" "-e" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold 'smart)
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool :case-fold 'smart)
       `(,@common-args "--heading" "--break" "-F" "-e" "Foo" ".")))
    (ert-info ("Group")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :group nil)
       `(,@common-args "-i" "-F" "-e" "foo" ".")))
    (ert-info ("Regexp")
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp t)
       `(,@common-args "--heading" "--break" "-i" "-G" "-e" "(foo)" "."))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'bre)
       `(,@common-args "--heading" "--break" "-i" "-G" "-e" "(foo)" "."))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'ere)
       `(,@common-args "--heading" "--break" "-i" "-E" "-e" "(foo)" "."))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'pcre)
       `(,@common-args "--heading" "--break" "-i" "-P" "-e" "(foo)" ".")))
    (ert-info ("Context")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context 3)
       `(,@common-args "--heading" "--break" "-C3" "-i" "-F" "-e" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(3 . 3))
       `(,@common-args "--heading" "--break" "-C3" "-i" "-F" "-e" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(2 . 4))
       `(,@common-args "--heading" "--break" "-B2" "-A4" "-i" "-F" "-e" "foo"
                       ".")))
    (ert-info ("Hidden files")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :hidden t)
       `(,@common-args "--hidden" "--heading" "--break" "-i" "-F" "-e" "foo"
                       ".")))
    (ert-info ("File wildcard")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard "*.el")
       `(,@common-args "--include=*.el" "--heading" "--break" "-i" "-F" "-e"
                       "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard '("*.c" "*.h"))
       `(,@common-args "--include=*.c" "--include=*.h" "--heading" "--break"
                       "-i" "-F" "-e" "foo" ".")))
    (ert-info ("Root")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "dir")
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo" "dir"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root '("dir1" "dir2"))
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo" "dir1"
                       "dir2"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "~/dir")
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo"
                       ,(expand-file-name "~/dir")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root nil)
       `(,@common-args "--heading" "--break" "-i" "-F" "-e" "foo")))
    (ert-info ("Color")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :color nil)
       '("ugrep" "--color=never" "-rn" "--ignore-files" "--heading"
         "--break" "-i" "-F" "-e" "foo" ".")))))

(ert-deftest urgrep-tests/command/ripgrep ()
  (let ((tool (assq 'ripgrep urgrep-tools))
        (common-args '("rg" "--color=always" "--colors=path:none"
                       "--colors=path:fg:magenta" "--colors=line:none"
                       "--colors=column:none" "--colors=match:none"
                       "--colors=match:fg:red" "--colors=match:style:bold"
                       "-n")))
    (ert-info ("String/case")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool)
       `(,@common-args "--heading" "-i" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool)
       `(,@common-args "--heading" "-F" "--" "Foo" "."))
      (let ((case-fold-search nil))
        (urgrep-tests/check-command
         (urgrep-command "foo" :tool tool)
         `(,@common-args "--heading" "-F" "--" "foo" ".")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold t)
       `(,@common-args "--heading" "-i" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold nil)
       `(,@common-args "--heading" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold 'smart)
       `(,@common-args "--heading" "-i" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool :case-fold 'smart)
       `(,@common-args "--heading" "-F" "--" "Foo" ".")))
    (ert-info ("Group")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :group nil)
       `(,@common-args "--no-heading" "-i" "-F" "--" "foo" ".")))
    (ert-info ("Regexp")
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp t)
       `(,@common-args "--heading" "-i" "--" "\\(foo\\)" "."))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'bre)
       `(,@common-args "--heading" "-i" "--" "\\(foo\\)" "."))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'ere)
       `(,@common-args "--heading" "-i" "--" "(foo)" "."))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'pcre)
       `(,@common-args "--heading" "-i" "--" "(foo)" ".")))
    (ert-info ("Context")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context 3)
       `(,@common-args "--heading" "-C3" "-i" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(3 . 3))
       `(,@common-args "--heading" "-C3" "-i" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(2 . 4))
       `(,@common-args "--heading" "-B2" "-A4" "-i" "-F" "--" "foo" ".")))
    (ert-info ("Hidden files")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :hidden t)
       `(,@common-args "--hidden" "--heading" "-i" "-F" "--" "foo" ".")))
    (ert-info ("File wildcard")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard "*.el")
       `(,@common-args "-g" "*.el" "--heading" "-i" "-F" "--" "foo" "."))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard '("*.c" "*.h"))
       `(,@common-args "-g" "*.c" "-g" "*.h" "--heading" "-i" "-F" "--" "foo"
                       ".")))
    (ert-info ("Root")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "dir")
       `(,@common-args "--heading" "-i" "-F" "--" "foo" "dir"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root '("dir1" "dir2"))
       `(,@common-args "--heading" "-i" "-F" "--" "foo" "dir1" "dir2"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "~/dir")
       `(,@common-args "--heading" "-i" "-F" "--" "foo"
                       ,(expand-file-name "~/dir")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root nil)
       `(,@common-args "--heading" "-i" "-F" "--" "foo")))
    (ert-info ("Color")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :color nil)
       '("rg" "--color=never" "-n" "--heading" "-i" "-F" "--" "foo" ".")))))

(ert-deftest urgrep-tests/command/ag ()
  (let ((tool (assq 'ag urgrep-tools))
        (common-args '("ag" "--color" "--color-path=35" "--color-line="
                       "--color-match=1;31")))
    (ert-info ("String/case")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool)
       `(,@common-args "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool)
       `(,@common-args "--group" "-s" "-Q" "--" "Foo"))
      (let ((case-fold-search nil))
        (urgrep-tests/check-command
         (urgrep-command "foo" :tool tool)
         `(,@common-args "--group" "-s" "-Q" "--" "foo")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold t)
       `(,@common-args "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold nil)
       `(,@common-args "--group" "-s" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold 'smart)
       `(,@common-args "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool :case-fold 'smart)
       `(,@common-args "--group" "-s" "-Q" "--" "Foo")))
    (ert-info ("Group")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :group nil)
       `(,@common-args "--nogroup" "-i" "-Q" "--" "foo")))
    (ert-info ("Regexp")
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp t)
       `(,@common-args "--group" "-i" "--" "\\(foo\\)"))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'bre)
       `(,@common-args "--group" "-i" "--" "\\(foo\\)"))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'ere)
       `(,@common-args "--group" "-i" "--" "(foo)"))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'pcre)
       `(,@common-args "--group" "-i" "--" "(foo)")))
    (ert-info ("Context")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context 3)
       `(,@common-args "--group" "-C3" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(3 . 3))
       `(,@common-args "--group" "-C3" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(2 . 4))
       `(,@common-args "--group" "-B2" "-A4" "-i" "-Q" "--" "foo")))
    (ert-info ("File wildcard")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard "*.el")
       `(,@common-args "-G" "^[^\\000]*\\.el$" "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard '("*.c" "*.h"))
       `(,@common-args "-G" "^[^\\000]*\\.(c|h)$" "--group" "-i" "-Q" "--"
                       "foo")))
    (ert-info ("Root")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "dir")
       `(,@common-args "--group" "-i" "-Q" "--" "foo" "dir"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root '("dir1" "dir2"))
       `(,@common-args "--group" "-i" "-Q" "--" "foo" "dir1" "dir2"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "~/dir")
       `(,@common-args "--group" "-i" "-Q" "--" "foo"
                       ,(expand-file-name "~/dir")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root nil)
       `(,@common-args "--group" "-i" "-Q" "--" "foo")))
    (ert-info ("Color")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :color nil)
       '("ag" "--nocolor" "--group" "-i" "-Q" "--" "foo")))))

(ert-deftest urgrep-tests/command/ack ()
  (let ((tool (assq 'ack urgrep-tools))
        (common-args '("ack" "--color" "--color-filename=magenta"
                       "--color-lineno=clear" "--color-colno=clear"
                       "--color-match=bold red"))
        (no-hidden-args '("--ignore-dir=match:/^\\./"
                          "--ignore-file=match:/^\\./")))
    (ert-info ("String/case")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool)
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool)
       `(,@common-args ,@no-hidden-args "--group" "-Q" "--" "Foo"))
      (let ((case-fold-search nil))
        (urgrep-tests/check-command
         (urgrep-command "foo" :tool tool)
         `(,@common-args ,@no-hidden-args "--group" "-Q" "--" "foo")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold t)
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold nil)
       `(,@common-args ,@no-hidden-args "--group" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold 'smart)
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool :case-fold 'smart)
       `(,@common-args ,@no-hidden-args "--group" "-Q" "--" "Foo")))
    (ert-info ("Group")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :group nil)
       `(,@common-args ,@no-hidden-args "--nogroup" "-i" "-Q" "--" "foo")))
    (ert-info ("Regexp")
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp t)
       `(,@common-args ,@no-hidden-args "--group" "-i" "--" "\\(foo\\)"))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'bre)
       `(,@common-args ,@no-hidden-args "--group" "-i" "--" "\\(foo\\)"))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'ere)
       `(,@common-args ,@no-hidden-args "--group" "-i" "--" "(foo)"))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'pcre)
       `(,@common-args ,@no-hidden-args "--group" "-i" "--" "(foo)")))
    (ert-info ("Context")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context 3)
       `(,@common-args ,@no-hidden-args "--group" "-C3" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(3 . 3))
       `(,@common-args ,@no-hidden-args "--group" "-C3" "-i" "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(2 . 4))
       `(,@common-args ,@no-hidden-args "--group" "-B2" "-A4" "-i" "-Q" "--"
                       "foo")))
    (ert-info ("Hidden files")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :hidden t)
       `(,@common-args "--group" "-i" "-Q" "--" "foo")))
    (ert-info ("File wildcard")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard "*.el")
       `(,@common-args ,@no-hidden-args "-G" "^[^\\000]*\\.el$" "--group" "-i"
                       "-Q" "--" "foo"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard '("*.c" "*.h"))
       `(,@common-args ,@no-hidden-args "-G" "^[^\\000]*\\.(c|h)$" "--group"
                       "-i" "-Q" "--" "foo")))
    (ert-info ("Root")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "dir")
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo" "dir"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root '("dir1" "dir2"))
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo" "dir1"
                       "dir2"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "~/dir")
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo"
                       ,(expand-file-name "~/dir")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root nil)
       `(,@common-args ,@no-hidden-args "--group" "-i" "-Q" "--" "foo")))
    (ert-info ("Color")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :color nil)
       `("ack" "--nocolor" ,@no-hidden-args "--group" "-i" "-Q" "--" "foo")))))

(ert-deftest urgrep-tests/command/git-grep ()
  (let ((tool (assq 'git-grep urgrep-tools))
        (common-args '("git" "--no-pager" "-c" "color.grep.filename=magenta"
                       "-c" "color.grep.match=bold red" "-c"
                       "color.grep.context=" "-c" "color.grep.function="
                       "-c" "color.grep.lineNumber=" "-c" "color.grep.column="
                       "-c" "color.grep.selected=" "-c" "color.grep.separator="
                       "grep" "--color" "--no-index" "--exclude-standard" "-n"))
        (group-args '("--heading" "--break"))
        (no-hidden-args '(":!.*")))
    (ert-info ("String/case")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool)
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool)
       `(,@common-args ,@group-args "-F" "-e" "Foo" "--" ,@no-hidden-args))
      (let ((case-fold-search nil))
        (urgrep-tests/check-command
         (urgrep-command "foo" :tool tool)
         `(,@common-args ,@group-args "-F" "-e" "foo" "--" ,@no-hidden-args)))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold t)
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold nil)
       `(,@common-args ,@group-args "-F" "-e" "foo" "--" ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :case-fold 'smart)
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "Foo" :tool tool :case-fold 'smart)
       `(,@common-args ,@group-args "-F" "-e" "Foo" "--" ,@no-hidden-args)))
    (ert-info ("Group")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :group nil)
       `(,@common-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args)))
    (ert-info ("Regexp")
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp t)
       `(,@common-args ,@group-args "-i" "-G" "-e" "(foo)" "--"
                       ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'bre)
       `(,@common-args ,@group-args "-i" "-G" "-e" "(foo)" "--"
                       ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'ere)
       `(,@common-args ,@group-args "-i" "-E" "-e" "(foo)" "--"
                       ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "(foo)" :tool tool :regexp 'pcre)
       `(,@common-args ,@group-args "-i" "-P" "-e" "(foo)" "--"
                       ,@no-hidden-args)))
    (ert-info ("Context")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context 3)
       `(,@common-args ,@group-args "-C3" "-i" "-F" "-e" "foo" "--"
                       ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(3 . 3))
       `(,@common-args ,@group-args "-C3" "-i" "-F" "-e" "foo" "--"
                       ,@no-hidden-args))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :context '(2 . 4))
       `(,@common-args ,@group-args "-B2" "-A4" "-i" "-F" "-e" "foo" "--"
                       ,@no-hidden-args)))
    (ert-info ("Hidden files")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :hidden t)
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--")))
    (ert-info ("File wildcard")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard "*.el")
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       "*.el"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard '("*.c" "*.h"))
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       "*.c" "*.h")))
    (ert-info ("Root")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "dir")
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       "dir"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root '("dir1" "dir2"))
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       "dir1" "dir2"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root "~/dir")
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       ,(expand-file-name "~/dir")))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :root nil)
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--"
                       ,@no-hidden-args)))
    (ert-info ("File wildcard + Directory")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard "*.el" :root "dir")
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       ":(glob)dir/**/*.el"))
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :file-wildcard '("*.c" "*.h")
                       :root '("dir1" "dir2"))
       `(,@common-args ,@group-args "-i" "-F" "-e" "foo" "--" ,@no-hidden-args
                       ":(glob)dir1/**/*.c" ":(glob)dir2/**/*.c"
                       ":(glob)dir1/**/*.h" ":(glob)dir2/**/*.h")))
    (ert-info ("Color")
      (urgrep-tests/check-command
       (urgrep-command "foo" :tool tool :color nil)
       `("git" "--no-pager" "grep" "--no-color" "--no-index"
         "--exclude-standard" "-n" ,@group-args "-i" "-F" "-e" "foo" "--"
         ,@no-hidden-args)))))

(ert-deftest urgrep-tests/command/grep ()
  (let ((tool (assq 'grep urgrep-tools))
        (template (concat "^find \\(\\|.+ \\)%s \\(\\|.+ \\)%s\\(\\|.+ \\)"
                          "grep %s\\(\\|.+ \\)%s"))
        (escape (lambda (i) (regexp-quote (shell-quote-argument i)))))
    (ert-info ("String/case")
      (should (string-match
               (format template "." "" "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool)))
      (should (string-match (format template "." "" "--color=always -F" "Foo")
                            (urgrep-command "Foo" :tool tool)))
      (let ((case-fold-search nil))
        (should (string-match (format template "." "" "--color=always -F" "foo")
                              (urgrep-command "foo" :tool tool))))
      (should (string-match
               (format template "." "" "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :case-fold t)))
      (should (string-match (format template "." "" "--color=always -F" "foo")
                            (urgrep-command "foo" :tool tool :case-fold nil)))
      (should (string-match
               (format template "." "" "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :case-fold 'smart)))
      (should (string-match
               (format template "." "" "--color=always -F" "Foo")
               (urgrep-command "Foo" :tool tool :case-fold 'smart))))
    (ert-info ("Group")
      (should (string-match (format template "" "" "--color=always -i -F" "foo")
                            (urgrep-command "foo" :tool tool :group nil))))
    (ert-info ("Regexp")
      (let ((query (funcall escape "(foo)")))
        (should (string-match
                 (format template "." "" "--color=always -i -G" query)
                 (urgrep-command "(foo)" :tool tool :regexp t)))
        (should (string-match
                 (format template "." "" "--color=always -i -G" query)
                 (urgrep-command "(foo)" :tool tool :regexp 'bre)))
        (should (string-match
                 (format template "." "" "--color=always -i -E" query)
                 (urgrep-command "(foo)" :tool tool :regexp 'ere)))
        (should (string-match
                 (format template "." "" "--color=always -i -P" query)
                 (urgrep-command "(foo)" :tool tool :regexp 'pcre)))))
    (ert-info ("Context")
      (should (string-match
               (format template "." "" "--color=always -C3 -i -F" "foo")
               (urgrep-command "foo" :tool tool :context 3)))
      (should (string-match
               (format template "." "" "--color=always -C3 -i -F" "foo")
               (urgrep-command "foo" :tool tool :context '(3 . 3))))
      (should (string-match
               (format template "." "" "--color=always -B2 -A4 -i -F" "foo")
               (urgrep-command "foo" :tool tool :context '(2 . 4)))))
    (ert-info ("File wildcard")
      (let ((escape (lambda (i) (regexp-quote (shell-quote-argument i)))))
        (should (string-match
                 (format template "."
                         (concat "-i?name " (funcall escape "*.el") " ")
                         "--color=always -i -F" "foo")
                 (urgrep-command "foo" :tool tool :file-wildcard "*.el")))
        (should (string-match
                 (format template "."
                         (concat "-i?name " (funcall escape "*.c") " -o "
                                 "-i?name " (funcall escape "*.h") " ")
                         "--color=always -i -F" "foo")
                 (urgrep-command "foo" :tool tool
                                 :file-wildcard '("*.c" "*.h"))))))
    (ert-info ("Root")
      (should (string-match
               (format template "dir" "" "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :root "dir")))
      (should (string-match
               (format template "dir1 dir2" "" "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :root '("dir1" "dir2"))))
      (should (string-match
               (format template (expand-file-name "~/dir") ""
                       "--color=always -i -F" "foo")
               (urgrep-command "foo" :tool tool :root "~/dir")))
      (should (string-match
               "grep  --color=always -i -F\\(\\|.+ \\)foo"
               (urgrep-command "foo" :tool tool :root nil))))
    (ert-info ("Color")
      (should (string-match (format template "." "" "+-i -F" "foo")
                            (urgrep-command "foo" :tool tool :color nil))))))

(ert-deftest urgrep-tests/get-tool/default ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--cached-tool)
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ugrep))
      (should (equal (urgrep--get-prop 'executable-name tool) "ugrep"))
      (should (equal urgrep--cached-tool tool)))))

(ert-deftest urgrep-tests/get-tool/default-cached ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((ag (assq 'ag urgrep-tools))
           (urgrep--cached-tool ag)
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "ag"))
      (should (equal urgrep--cached-tool ag)))))

(ert-deftest urgrep-tests/get-tool/preferred ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--cached-tool)
           (urgrep-preferred-tools '(ag grep))
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "ag"))
      (should (equal urgrep--cached-tool tool)))))

(ert-deftest urgrep-tests/get-tool/preferred-cons ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--cached-tool)
           (urgrep-preferred-tools '((ag . "/usr/bin/ag")))
           (tool (urgrep-get-tool)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "/usr/bin/ag"))
      (should (equal urgrep--cached-tool tool)))))

(ert-deftest urgrep-tests/get-tool/preferred-buffer-local ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--cached-tool)
           (urgrep-preferred-tools '(ag grep)))
      (should (equal (car (urgrep-get-tool)) 'ag))
      (with-temp-buffer
        (setq-local urgrep-preferred-tools '(ripgrep))
        (should (equal (car (urgrep-get-tool)) 'ripgrep)))
      (should (equal (car (urgrep-get-tool)) 'ag)))))

(ert-deftest urgrep-tests/get-tool/key ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--cached-tool)
           (tool (urgrep-get-tool 'ag)))
      (should (equal (car tool) 'ag))
      (should (equal (urgrep--get-prop 'executable-name tool) "ag"))
      (should (equal urgrep--cached-tool nil)))))

(ert-deftest urgrep-tests/get-tool/cons ()
  (cl-letf (((symbol-function #'executable-find) #'always))
    (let* ((urgrep--cached-tool)
           (tool (urgrep-get-tool '(goofy (executable-name . "gf")))))
      (should (equal (car tool) 'goofy))
      (should (equal (urgrep--get-prop 'executable-name tool) "gf"))
      (should (equal urgrep--cached-tool nil)))))

(ert-deftest urgrep-tests/guess-tool/simple ()
  (should (equal (car (urgrep--guess-tool "ag query")) 'ag))
  (should (equal (car (urgrep--guess-tool "rg query")) 'ripgrep)))

(ert-deftest urgrep-tests/guess-tool/list ()
  (should (equal (car (urgrep--guess-tool "find query")) 'grep)))

(ert-deftest urgrep-tests/guess-tool/fully-qualified ()
  (should (equal (car (urgrep--guess-tool "/usr/bin/ag query")) 'ag))
  (should (equal (car (urgrep--guess-tool "/home/me/bin/rg query")) 'ripgrep)))

(ert-deftest urgrep-tests/guess-tool/error ()
  (should-error (urgrep--guess-tool "goofy query")))

(ert-deftest urgrep-tests/get-tool/remote-host ()
  (skip-unless (urgrep-tests/remote-accessible-p))
  (defvar ert-remote-temporary-file-directory)
  (connection-local-set-profile-variables
   'urgrep-test-ripgrep
   '((urgrep-preferred-tools . (ripgrep))))
  (let ((default-directory ert-remote-temporary-file-directory))
    (connection-local-set-profiles
     (connection-local-criteria-for-default-directory) 'urgrep-test-ripgrep))
  (cl-letf (((symbol-function #'executable-find) #'always)
            (urgrep--cached-tool nil))
    ;; Get the preferred tool on the local host.
    (let ((tool (with-connection-local-variables (urgrep-get-tool))))
      (should (equal (car tool) 'ugrep))
      (should (equal (urgrep--get-prop 'executable-name tool) "ugrep")))
    ;; Now try on a remote host.
    (let* ((default-directory ert-remote-temporary-file-directory)
           (tool (with-connection-local-variables (urgrep-get-tool))))
      (should (equal (car tool) 'ripgrep))
      (should (equal (urgrep--get-prop 'executable-name tool) "rg")))
    ;; Try again on the local host to make sure it didn't change.
    (let ((tool (with-connection-local-variables (urgrep-get-tool))))
      (should (equal (car tool) 'ugrep))
      (should (equal (urgrep--get-prop 'executable-name tool) "ugrep")))))

(ert-deftest urgrep-tests/urgrep/group ()
  (switch-to-buffer (urgrep "urgrep"))
  (while (get-buffer-process (current-buffer))
    (accept-process-output))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query '("urgrep"))
               (local-variable-p 'urgrep-current-query)))
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests/check-match-at-point))

(ert-deftest urgrep-tests/urgrep/no-group ()
  (switch-to-buffer (urgrep "urgrep" :group nil))
  (while (get-buffer-process (current-buffer))
    (accept-process-output))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query '("urgrep" :group nil))
               (local-variable-p 'urgrep-current-query)))
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el:")
  (urgrep-tests/check-match-at-point))

(ert-deftest urgrep-tests/urgrep/repeat-from-urgrep ()
  (let ((base-name (file-name-nondirectory
                    (directory-file-name default-directory)))
        (parent-dir (file-name-parent-directory default-directory)))
    (message "TEST1: %S" default-directory)
    ;; Run Urgrep once in the current directory.
    (switch-to-buffer (urgrep "urgrep"))
    (message "TEST2: %S" default-directory)
    (while (get-buffer-process (current-buffer))
      (accept-process-output))
    (message "TEST3: %S" default-directory)
    (message "TEST4: %S %S" default-directory parent-dir)
    ;; Run Urgrep again, starting in the *urgrep* buffer, but change the
    ;; directory to search in.
    (urgrep "urgrep-tests/" :default-directory parent-dir)
    (while (get-buffer-process (current-buffer))
      (accept-process-output))
    (should (and (equal urgrep-current-query '("urgrep-tests/"))
                 (local-variable-p 'urgrep-current-query)))
    (should (equal default-directory parent-dir))
    (goto-char (point-min))
    (re-search-forward (concat base-name "/urgrep-tests.el"))
    (beginning-of-line 2)
    (urgrep-tests/check-match-at-point
     (concat base-name "/urgrep-tests.el"))))

(ert-deftest urgrep-tests/urgrep-run-command ()
  (switch-to-buffer (urgrep-run-command (urgrep-command "urgrep")))
  (while (get-buffer-process (current-buffer))
    (accept-process-output))
  (should (and (equal urgrep-current-tool (urgrep-get-tool))
               (local-variable-p 'urgrep-current-tool)))
  (should (and (equal urgrep-current-query (urgrep-command "urgrep"))
               (local-variable-p 'urgrep-current-query)))
  (goto-char (point-min))
  (re-search-forward "urgrep-tests.el")
  (beginning-of-line 2)
  (urgrep-tests/check-match-at-point))

(ert-deftest urgrep-tests/eshell/urgrep-buffer ()
  (with-temp-eshell
   (eshell-match-command-output "urgrep universal" "\\`#<buffer .*>\n")
   (with-current-buffer eshell-last-command-result
     (while (get-buffer-process (current-buffer))
       (accept-process-output))
     (should (eq major-mode #'urgrep-mode)))))

(ert-deftest urgrep-tests/eshell/inline ()
  (skip-unless (executable-find "cat"))
  (with-temp-eshell
   (eshell-match-command-output "urgrep universal | cat" "^README.md\n")))

;;; urgrep-tests.el ends here
