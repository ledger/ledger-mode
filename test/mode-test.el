;;; mode-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-mode

;;; Code:
(require 'test-helper)


(ert-deftest ledger-mode/test-001 ()
  "Test ledger-read-account-with-prompt (used in ledger-reconcile)"
  :tags '(mode baseline interactive)

  (ledger-tests-with-temp-file
   demo-ledger

   (goto-char (point-min))              ; beginning-of-buffer
   ;; See http://stackoverflow.com/questions/32961823/how-can-i-test-an-interactive-function-in-emacs
   ;; for an explanation of unread-command-events trick
   (should (string= ""
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   (forward-line)
   (should (string= "Assets:Checking"
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   (forward-line)
   (should (string= "Equity:Opening Balances"
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   (forward-line)
   (should (string= ""
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   ))


(ert-deftest ledger-mode/test-002 ()
  "Regress test for Bug 256
http://bugs.ledger-cli.org/show_bug.cgi?id=256"
  :tags '(mode regress)

  (ledger-tests-with-temp-file
   ""

   (comment-dwim nil)
   (should (equal (buffer-string) "; ")) ; Expected: no space before ';'
   ))


(ert-deftest ledger-mode/test-003 ()
  "Baseline test for comment-start"
  :tags '(mode baseline)

  (ledger-tests-with-temp-file
   ""

   (setq comment-start "#")
   (comment-dwim nil)
   (should (equal (buffer-string) "# "))
   ))


(provide 'mode-test)

;;; mode-test.el ends here
