;;; exec-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 John Wiegley <johnw AT gnu DOT org>

;; Author: Thierry <thdox AT free DOT fr>
;; Keywords: languages
;; Homepage: https://github.com/ledger/ledger-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  Regression tests for ledger-exec

;;; Code:
(require 'subr-x)
(require 'ledger-exec)
(require 'test-helper)


(ert-deftest ledger-exec/test-001 ()
  "Regress test for Bug 254
http://bugs.ledger-cli.org/show_bug.cgi?id=254"
  :tags '(exec regress)

  (ledger-tests-with-temp-file
      ""
    (ledger-check-version)
    (should
     (eq t ledger-works))))


(ert-deftest ledger-exec/test-handle-error ()
  "`ledger-exec-handle-error' loads error contents into the *Ledger Error* buffer."
  :tags '(exec)
  (let ((tmp (make-temp-file "ledger-error-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "While parsing input:\nSyntax error\n"))
          (let ((buf (ledger-exec-handle-error tmp)))
            (unwind-protect
                (with-current-buffer buf
                  (should (string-match-p "Syntax error" (buffer-string)))
                  (should buffer-read-only))
              (kill-buffer buf))))
      (delete-file tmp))))


(ert-deftest ledger-exec/test-not-executable ()
  "`ledger-exec-ledger' signals an error when the binary path is not executable."
  :tags '(exec)
  (let ((ledger-binary-path "/no/such/binary-that-does-not-exist"))
    (with-temp-buffer
      (should-error (ledger-exec-ledger (current-buffer))))))


(ert-deftest ledger-exec/test-execution-failure ()
  "`ledger-exec-ledger' signals an error when ledger exits non-zero."
  :tags '(exec)
  (with-temp-buffer
    (insert "this is not a valid ledger file at all\n!@#$%\n")
    (should-error
     (let ((display-buffer-alist '(("." (display-buffer-no-window))))
           (inhibit-message t))
       (ledger-exec-ledger (current-buffer) nil "balance"))))
  ;; The error buffer should have been created.
  (when-let* ((buf (get-buffer "*Ledger Error*")))
    (kill-buffer buf)))


(ert-deftest ledger-exec/test-bad-version ()
  "`ledger-check-version' messages \"Bad Ledger Version\" when the binary fails."
  :tags '(exec)
  (cl-letf (((symbol-function 'ledger-version-greater-p)
             (lambda (_needed) nil)))
    (ledger-tests-with-temp-file ""
      (let ((ledger-mode-should-check-version t)
            (ledger-works t))
        (ledger-check-version)
        (should (eq nil ledger-works))))))


(ert-deftest ledger-exec/test-absolute-binary-path ()
  "`ledger-exec-ledger' accepts an absolute, executable binary path."
  :tags '(exec)
  (let* ((found (executable-find ledger-binary-path))
         (ledger-binary-path found))
    (skip-unless found)
    (with-temp-buffer
      (insert "")
      (let ((buf (ledger-exec-ledger (current-buffer) nil "--version")))
        (should (bufferp buf))
        (kill-buffer buf)))))


(ert-deftest ledger-exec/test-default-input-buffer ()
  "When no INPUT-BUFFER is given, `ledger-exec-ledger' uses `ledger-master-file'."
  :tags '(exec)
  (let* ((tmp (make-temp-file "ledger-master-" nil ".dat"))
         (ledger-exec--args-only t))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "2024/01/01 Foo\n  A  $1\n  B\n"))
          (cl-letf (((symbol-function 'ledger-master-file)
                     (lambda () tmp)))
            (let ((buf (ledger-exec-ledger nil nil "balance")))
              (should (bufferp buf))
              (kill-buffer buf))))
      (delete-file tmp)
      ;; Kill the master-file buffer that may have been created.
      (when-let* ((b (get-file-buffer tmp)))
        (kill-buffer b)))))


(ert-deftest ledger-exec/test-success-empty-buffer ()
  "`ledger-exec-success-p' returns non-nil when the output buffer is essentially empty."
  :tags '(exec)
  (with-temp-buffer
    (should (ledger-exec-success-p 0 (current-buffer)))
    (insert "x")
    ;; Buffer is too small (size==1) to trigger While check, still success.
    (should (ledger-exec-success-p 0 (current-buffer)))
    (erase-buffer)
    (insert "While parsing\nfoo")
    (should-not (ledger-exec-success-p 0 (current-buffer)))))


(provide 'exec-test)

;;; exec-test.el ends here
