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
  :tags '(mode baseline)

  (ledger-tests-with-temp-file
      demo-ledger

    (goto-char (point-min))              ; beginning-of-buffer
    (should (string= ""
                     (ledger-tests-with-simulated-input "RET"
                       (ledger-read-account-with-prompt "Account to reconcile"))))
    (forward-line)
    (should (string= "Assets:Checking"
                     (ledger-tests-with-simulated-input "RET"
                       (ledger-read-account-with-prompt "Account to reconcile"))))
    (forward-line)
    (should (string= "Equity:Opening Balances"
                     (ledger-tests-with-simulated-input "RET"
                       (ledger-read-account-with-prompt "Account to reconcile"))))
    (forward-line)
    (should (string= ""
                     (ledger-tests-with-simulated-input "RET"
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

(ert-deftest ledger-mode/test-008 ()
  "Baseline test for `ledger-insert-effective-date'."
  :tags '(mode baseline)

  (let ((orig-file-contents
         "\
2024-01-01 Grocery Store
    Expenses:Groceries                           $30
    Liabilities:Credit Card

2024-01-02 Grocery Store
    Expenses:Groceries                           $10
    Expenses:Tax                                  $1.50
    Liabilities:Credit Card                     -$11.50
"))
    (ledger-tests-with-temp-file
        orig-file-contents

      ;; insert effective date for xact
      (ledger-insert-effective-date "2024-01-02")
      (should (equal
               (buffer-string)
               "\
2024-01-01=2024-01-02 Grocery Store
    Expenses:Groceries                           $30
    Liabilities:Credit Card

2024-01-02 Grocery Store
    Expenses:Groceries                           $10
    Expenses:Tax                                  $1.50
    Liabilities:Credit Card                     -$11.50
"))

      ;; With prefix arg, remove the effective date
      (let ((current-prefix-arg '(4)))
        (call-interactively 'ledger-insert-effective-date))
      (should (equal (buffer-string) orig-file-contents))

      ;; insert effective date after posting with no amount
      (forward-line 2)
      (ledger-insert-effective-date "2024-01-02")
      (should (equal
               (buffer-string)
               "\
2024-01-01 Grocery Store
    Expenses:Groceries                           $30
    Liabilities:Credit Card  ; [=2024-01-02]

2024-01-02 Grocery Store
    Expenses:Groceries                           $10
    Expenses:Tax                                  $1.50
    Liabilities:Credit Card                     -$11.50
"))
      (should (eolp))
      (beginning-of-line)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'ledger-insert-effective-date))
      (should (equal (buffer-string) orig-file-contents))

      ;; insert effective date after posting with amount
      (forward-line 5)
      (ledger-insert-effective-date "2024-01-03")
      (should (equal
               (buffer-string)
               "\
2024-01-01 Grocery Store
    Expenses:Groceries                           $30
    Liabilities:Credit Card

2024-01-02 Grocery Store
    Expenses:Groceries                           $10
    Expenses:Tax                                  $1.50
    Liabilities:Credit Card                     -$11.50  ; [=2024-01-03]
"))
      (should (eolp))
      (let ((current-prefix-arg '(4)))
        (call-interactively 'ledger-insert-effective-date))
      (should (equal (buffer-string) orig-file-contents)))))

(ert-deftest ledger-mode/test-009 ()
  "Baseline test for `ledger-insert-effective-date-region'."
  :tags '(mode baseline)

  (cl-flet ((file-with-dates (date-1 date-2)
              ;; A file with two optional effective dates.
              (format
                "\
2024-01-01%s Grocery Store
    Expenses:Groceries                           $30
    Liabilities:Credit Card

2024-01-02%s Grocery Store
    Expenses:Groceries                           $10
    Expenses:Tax                                  $1.50
    Liabilities:Credit Card                     -$11.50
"
                (if date-1 (concat "=" date-1) "")
                (if date-2 (concat "=" date-2) ""))))
    (ledger-tests-with-temp-file
      (file-with-dates nil nil)

      ;; With no prefix arg, insert or replace effective date for xacts that
      ;; start on a line that overlaps the range. With one, remove the
      ;; effective date.

      ;; Range fully contained in start line.
      (save-excursion
        (let ((min (progn (forward-char 1) (point)))
              (max (progn (forward-char 1) (point))))
          (ledger-insert-effective-date-region min max "2024-03-01")
          (should (equal (buffer-string) (file-with-dates "2024-03-01" nil)))))

      ;; Range doesn't overlap the start of an xact: don't do anything.
      (save-excursion
        (let ((min (progn (forward-line 1) (point)))
              (max (progn (forward-line 3) (point))))
          (ledger-insert-effective-date-region min max "2024-03-02")
          (should (equal (buffer-string) (file-with-dates "2024-03-01" nil)))))

      ;; Range overlaps multiple xacts: update them all.
      (save-excursion
        (let ((min (point))
              (max (progn (forward-line 4) (forward-char 1) (point))))
          (ledger-insert-effective-date-region min max "2024-03-03")
          (should (equal (buffer-string)
                         (file-with-dates "2024-03-03" "2024-03-03")))))

      ;; Remove effective date from multiple.
      (save-excursion
        (let ((min (point))
              (max (progn (forward-line 4) (forward-char 1) (point)))
              (current-prefix-arg '(4)))
          (ledger-insert-effective-date-region min max nil)
          (should (equal (buffer-string) (file-with-dates nil nil)))))

      ;; Add it back to both.
      (save-excursion
        (let ((min (point))
              (max (progn (forward-line 4) (forward-char 1) (point))))
          (ledger-insert-effective-date-region min max "2024-03-04")
          (should (equal (buffer-string)
                         (file-with-dates "2024-03-04" "2024-03-04")))))

      ;; Remove from just the first.
      (save-excursion
        (let ((min (progn (forward-char 1) (point)))
              (max (progn (forward-char 1) (point)))
              (current-prefix-arg '(4)))
          (ledger-insert-effective-date-region min max nil)
          (should (equal (buffer-string)
                         (file-with-dates nil "2024-03-04"))))))))

(provide 'mode-test)

;;; mode-test.el ends here
