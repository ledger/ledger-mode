;;; navigate-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-navigate

;;; Code:
(require 'test-helper)


(ert-deftest ledger-navigate/test-001 ()
  "Regress test for Bug 441
http://bugs.ledger-cli.org/show_bug.cgi?id=441"
  :tags '(navigate regress)

  (ledger-tests-with-temp-file
   demo-ledger
   (goto-char (point-min))              ; beginning-of-buffer
   (ledger-navigate-next-xact-or-directive)
   (ledger-navigate-next-xact-or-directive)
   (should (eq 556 (point)))
   (ledger-navigate-prev-xact-or-directive)
   (should (eq 104 (point)))))

(ert-deftest ledger-navigate/test-002 ()
  "Testing ledger-point-on-valid-xact-p"
  :tags '(navigate baseline)
  (ledger-tests-with-temp-file
      "2023/09/25 Payee 1
    Expenses  $10.00
    Assets:Checking
2023/09/25 * Payee 2
    Expenses  $10.00
    Assets:Checking
2023/09/25 ! Payee 3
    Expenses  $10.00
    Assets:Checking
= expr true
    Expenses  $10.00
    Assets:Checking  -$10.00

~ Monthly
    Expenses  $10.00
    Assets:Checking  $-10.00
tag test
year 2023
comment
2023/09/25 Payee 4
    Expenses  $10.00
    Assets:Checking
end comment
;2023/09/25 Payee 5
;    Expenses  $10.00
;    Assets:Checking"
    (goto-char (point-min))
    (should (eq t (ledger-point-on-valid-xact-p))) ; Payee 1
    (forward-line)
    (should (eq t (ledger-point-on-valid-xact-p))) ; On a transaction posting
    (ledger-navigate-next-xact-or-directive)
    (should (eq t (ledger-point-on-valid-xact-p))) ; Payee 2 (cleared)
    (ledger-navigate-next-xact-or-directive)
    (should (eq t (ledger-point-on-valid-xact-p))) ; Payee 3 (pending)
    (ledger-navigate-next-xact-or-directive)
    (should (eq t (ledger-point-on-valid-xact-p))) ; Automated transaction
    (forward-line 3)
    (should (eq nil (ledger-point-on-valid-xact-p))) ; Blank line
    (ledger-navigate-next-xact-or-directive)
    (should (eq t (ledger-point-on-valid-xact-p))) ; Periodic transaction
    (ledger-navigate-next-xact-or-directive)
    (should (eq nil (ledger-point-on-valid-xact-p))) ; On a tag directive
    (ledger-navigate-next-xact-or-directive)
    (should (eq nil (ledger-point-on-valid-xact-p))) ; On a year directive
    (ledger-navigate-next-xact-or-directive)
    (should (eq nil (ledger-point-on-valid-xact-p))) ; On a comment block directive
    (ledger-navigate-next-xact-or-directive)
    (should (eq t (ledger-point-on-valid-xact-p))) ; On a transaction within a comment block
    (ledger-navigate-next-xact-or-directive)
    (should (eq nil (ledger-point-on-valid-xact-p))) ; On an end-comment directive
    (ledger-navigate-next-xact-or-directive)
    (should (eq nil (ledger-point-on-valid-xact-p))) ; On a ;-commented transaction
    (ledger-navigate-next-xact-or-directive)
    (should (eq nil (ledger-point-on-valid-xact-p))))) ; On a ;-commented transaction posting

(ert-deftest ledger-navigate/test-003 ()
  "Testing ledger-navigate-beginning-of-xact"
  :tags '(navigate baseline)
  (ledger-tests-with-temp-file
      "2023/09/25 Payee
Expenses  $10.00
Assets:Checking
; This line is not a valid transaction
2023/09/25 Payee 2
Expenses  $10.00
Assets:Checking"
    ;; First test that the function works on a regular transaction
    (goto-char (point-min))
    (forward-line 4)
    (ledger-navigate-beginning-of-xact)
    (should (eq 90 (point)))
    ;; Next, test that the function does not move point when it fails
    (forward-line -1)
    (forward-char 2)
    (ignore-errors (ledger-navigate-beginning-of-xact))
    (should (eq 53 (point)))))

(ert-deftest ledger-navigate-uncleared ()
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "2011/01/27 Book Store
    Expenses:Books                       $20.00
    Liabilities:MasterCard

2011/04/25 * Tom's Used Cars
    Expenses:Auto                    $ 5,500.00
    Assets:Checking

2011/04/27 Bookstore
    Expenses:Books                       $20.00
    Assets:Checking

2011/12/01 * Sale
    Assets:Checking                     $ 30.00
    Income:Sales"
   (ledger-navigate-next-uncleared)
   (should (looking-at-p (regexp-quote "2011/04/27 Bookstore")))
   (should-error (ledger-navigate-next-uncleared))
   (ledger-navigate-previous-uncleared)
   (should (bobp))))


(provide 'navigate-test)

;;; navigate-test.el ends here
