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
  (ledger-tests-with-temp-file ""
    (comment-dwim nil)
    (should (string-match (rx buffer-start ";" (0+ whitespace))
                          ;; Expected: no space before ';'
                          (buffer-string)))))

(ert-deftest ledger-mode/test-003 ()
  "Baseline test for comment-start"
  :tags '(mode baseline)
  (ledger-tests-with-temp-file ""
    (setq comment-start "#")
    (comment-dwim nil)
    (should (string-match (rx buffer-start "#" (0+ whitespace)) (buffer-string)))))


(ert-deftest ledger-mode/test-004 ()
  "Baseline test for `ledger-rename-account'."
  :tags '(mode baseline)
  (ledger-tests-with-temp-file
      "2024-04-01 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"
    (save-buffer)

    (ledger-rename-account
     "Expenses:Groceries"
     "Expenses:Grocery")

    (should
     (equal (buffer-string)
            "2024-04-01 Grocery Store
    Expenses:Grocery                             $30
    Expenses:Grocery:Snacks                      $10
    Assets:Cash
"))

    (revert-buffer t t)
    (ledger-rename-account
     "Expenses:Groceries"
     "Expenses:Grocery"
     'toplevel-only)

    (should
     (equal (buffer-string)
            "2024-04-01 Grocery Store
    Expenses:Grocery                             $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"))))

(ert-deftest ledger-mode/test-005 ()
  "Baseline test for `ledger-date-up' and `ledger-date-down'."
  :tags '(mode baseline)
  (ledger-tests-with-temp-file
      "2024-12-31 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"

    ;; Verify that month & year change accordingly when day changes.
    (goto-char 10)
    (call-interactively #'ledger-date-up)
    (should
     (equal (buffer-string)
            "2025-01-01 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"))

    ;; Verify that month & year change accordingly when day changes.
    (ledger-date-down 1)
    (should
     (equal (buffer-string)
            "2024-12-31 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"))

    ;; Verify that the field at point (not always day) is changed
    (goto-char 2)
    (ledger-date-down 2)

    (should
     (equal (buffer-string)
            "2022-12-31 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"))))

(ert-deftest ledger-mode/test-006 ()
  "Test for `ledger-date-up' and `ledger-date-down' with effective dates."
  :tags '(mode baseline)
  (ledger-tests-with-temp-file
      "2024-12-31=2025-01-01 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"

    (goto-char 10)
    (ledger-date-up 2)
    (should
     (equal (buffer-string)
            "2025-01-02=2025-01-01 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"))

    (goto-char 21)
    (ledger-date-down 3)
    (should
     (equal (buffer-string)
            "2025-01-02=2024-12-29 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"))))

(ert-deftest ledger-mode/test-007 ()
  "Test for `ledger-date-up' and `ledger-date-down' across DST boundaries."
  :tags '(mode baseline)
  (ledger-tests-with-time-zone "America/New_York"
    (ledger-tests-with-temp-file
        "2024-03-11 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
"

      (goto-char 6)
      (ledger-date-down 1)
      (should
       (equal (buffer-string)
              "2024-02-11 Grocery Store
    Expenses:Groceries                           $30
    Expenses:Groceries:Snacks                    $10
    Assets:Cash
")))))

(provide 'mode-test)

;;; mode-test.el ends here
