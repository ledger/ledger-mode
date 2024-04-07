;;; complete-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-complete

;;; Code:
(require 'test-helper)


(ert-deftest ledger-complete/test-001 ()
  "Regress test for Bug 969+582
http://bugs.ledger-cli.org/show_bug.cgi?id=969
http://bugs.ledger-cli.org/show_bug.cgi?id=582"
  :tags '(complete regress)

  (let ((ledger-complete-in-steps t))
    (ledger-tests-with-temp-file
        "2013/05/19 Retrait
    Dépense:Alimentation:Épicerie  35 €  ; Marché
    Dépense:Alimentation:Épicerie  8,1 €  ; Arum café
    Dépense:Liquide
    * Passif:Crédit:BanqueAccord              -60,00 €"
      (forward-line 1)
      (move-end-of-line 1)
      (newline)
      (insert "    Dé")
      (call-interactively #'completion-at-point)
      (should
       (equal (buffer-string)
              "2013/05/19 Retrait
    Dépense:Alimentation:Épicerie  35 €  ; Marché
    Dépense:
    Dépense:Alimentation:Épicerie  8,1 €  ; Arum café
    Dépense:Liquide
    * Passif:Crédit:BanqueAccord              -60,00 €")))))


(ert-deftest ledger-complete/test-002 ()
  "Regress test for Bug 252
http://bugs.ledger-cli.org/show_bug.cgi?id=252"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2010/04/08 payee
    account1                1 €
    account2
"
    (goto-char (point-max))
    (newline)
    (insert "2016/09/01 payee")
    (ledger-fully-complete-xact)
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "2010/04/08 payee
    account1                1 €
    account2

2016/09/01 payee
    account1                1 €
    account2
"))
    (ledger-navigate-beginning-of-xact)
    (open-line 1)
    (insert "2015/08/01 payee")
    (ledger-fully-complete-xact)
    (should
     (equal (buffer-substring-no-properties (point-min) (point-max))
            "2010/04/08 payee
    account1                1 €
    account2

2015/08/01 payee
    account1                1 €
    account2

2016/09/01 payee
    account1                1 €
    account2
"            ))))

(ert-deftest ledger-complete/test-complete-virtual-account-brackets ()
  "https://github.com/ledger/ledger-mode/issues/141"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    [blo"
    (goto-char (point-max))
    (call-interactively 'completion-at-point)
    (should
     (equal (buffer-string)
            "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    [bloop"))))

(ert-deftest ledger-complete/test-complete-virtual-account-parens ()
  "https://github.com/ledger/ledger-mode/issues/141"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    (blo"
    (goto-char (point-max))
    (call-interactively 'completion-at-point)
    (should
     (equal (buffer-string)
            "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    (bloop"))))

(ert-deftest ledger-complete/test-complete-account-without-amount ()
  "https://github.com/ledger/ledger-mode/issues/141"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    blo"
    (goto-char (point-max))
    (call-interactively 'completion-at-point)
    (should
     (equal (buffer-string)
            "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    bloop"))))

(ert-deftest ledger-complete/test-complete-single-payee ()
  "https://github.com/ledger/ledger-mode/issues/181"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
   "2019/06/28 Foobar
    Expenses:Baz                               11.99 CAD
    Assets:Cash

2019/06/20 Foo"
   (goto-char (point-max))
   (call-interactively 'completion-at-point)
   (should
    (equal (buffer-string)
           "2019/06/28 Foobar
    Expenses:Baz                               11.99 CAD
    Assets:Cash

2019/06/20 Foobar"))))

(ert-deftest ledger-complete/test-complete-declared-payee ()
  "https://github.com/ledger/ledger-mode/issues/181"
  :tags '(complete)
  (ledger-tests-with-temp-file
   "payee Foobar
2019/06/28 F"
   (goto-char (point-max))
   (call-interactively 'completion-at-point)
   (should
    (equal (buffer-string)
           "payee Foobar
2019/06/28 Foobar"))))

(eval-when-compile
  (when (< emacs-major-version 28)
    (defvar inhibit-interaction)))

(ert-deftest ledger-complete/test-complete-payee-multiple-words ()
  "Regression test for #420.
https://github.com/ledger/ledger-mode/issues/420"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
   "payee Foo Bar
payee Bar Baz

2019/06/28 Foo B"
   (goto-char (point-max))
   (let ((inhibit-interaction t))       ;require a unique match
     (completion-at-point))
   (should
    (equal (buffer-string)
           "payee Foo Bar
payee Bar Baz

2019/06/28 Foo Bar"))))

(ert-deftest ledger-complete/test-find-accounts-in-buffer ()
  :tags '(complete)
  (let ((ledger "*** Expenses
account Expenses:Accommodation
account Assets:Cash  ; some comment
account Assets:Current
    alias 1187465S022
commodity EUR
    format 1,000.00 EUR
tag ofxid
2018/05/07 * Company
    Assets:Current  -38.33 EUR
    ; ofxid: someid
    Expenses:Utilities:Insurance  38.00 EUR
    [Dimensions:Foo]  30.00 EUR
    [Expenses:Accommodation]  8.33 EUR
    [Dimensions:Equity]  -38.33 EUR
    (Something)  43.00 EUR
"))
    (with-temp-buffer
      (insert ledger)
      (should (equal
               (ledger-accounts-list-in-buffer)
               (list
                "Assets:Cash"
                "Assets:Current"
                "Dimensions:Equity"
                "Dimensions:Foo"
                "Expenses:Accommodation"
                "Expenses:Utilities:Insurance"
                "Something"))))))

(ert-deftest ledger-complete/test-find-accounts-with-spaces-in-buffer ()
  :tags '(complete)
  (let ((ledger "*** Expenses
account Expenses:The Bakery
"))
    (with-temp-buffer
      (insert ledger)
      (should (equal
               (ledger-accounts-list-in-buffer)
               (list
                "Expenses:The Bakery"))))))

(ert-deftest ledger-complete/test-ledger-accounts-exclude-function ()
  :tags '(complete)
  (with-temp-buffer
    (insert "account Assets:Checking:Bank A
    assert date<=[1990-01-01]
account Assets:Checking:Bank B")
    (let ((ledger-accounts-exclude-function
           (lambda (i) "Exclude all entries with a subdirective."
             (cdr i))))
      (should (equal (ledger-accounts-list-in-buffer)
                     (list "Assets:Checking:Bank B"))))
    (let ((ledger-accounts-exclude-function (lambda (_) t)))
      (should (equal (ledger-accounts-list-in-buffer)
                     nil)))
    (let ((ledger-accounts-exclude-function (lambda (_) nil)))
      (should (equal (ledger-accounts-list-in-buffer)
                     (list "Assets:Checking:Bank A"
                           "Assets:Checking:Bank B"))))))

(ert-deftest ledger-complete/test-account-completion-in-steps ()
  :tags '(complete)
  (let ((completion-cycle-threshold t)
        (ledger-complete-in-steps t))
    (ledger-tests-with-temp-file
        "2020-01-01 Opening Balances
    Assets:Bank:Balance                       100.00 EUR
    Equity:Opening Balances

2020-02-01 Acme Widgetry GmbH
    Assets:Bank:Deposit:20200201              200.00 EUR
    Income:Salary

2020-03-01 Gear
    Assets:Reimbursements                      50.00 EUR
    Assets:Bank:Balance

2020-04-01 Fnord
    As"
      (goto-char (point-max))
      (call-interactively 'completion-at-point)
      (should
       (equal (buffer-substring-no-properties (point-min) (point-max))
              "2020-01-01 Opening Balances
    Assets:Bank:Balance                       100.00 EUR
    Equity:Opening Balances

2020-02-01 Acme Widgetry GmbH
    Assets:Bank:Deposit:20200201              200.00 EUR
    Income:Salary

2020-03-01 Gear
    Assets:Reimbursements                      50.00 EUR
    Assets:Bank:Balance

2020-04-01 Fnord
    Assets:")))))

(ert-deftest ledger-complete/amount-separated-by-tab ()
  "https://github.com/ledger/ledger-mode/issues/339"
  :tags '(complete regress)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2019/06/28 Foobar
\tExpenses\t11.99 CAD
\tEx\t-11.99 CAD"
      (forward-line 2)
      (forward-word 1)
      (call-interactively 'completion-at-point)
      (should
       (equal (buffer-string)
              "2019/06/28 Foobar
\tExpenses\t11.99 CAD
\tExpenses\t-11.99 CAD")))))

(defvar ledger-flymake-be-pedantic)

(ert-deftest ledger-complete/accounts-list-in-buffer ()
  "https://github.com/ledger/ledger-mode/issues/380"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "\
account Expenses:Food
    note Use for dining out, etc.

account Expenses:Groceries
    note Use for food I cook myself

2023-12-04 Restaurant
\tExpenses:Food\t30 USD
\tLiabilities:Credit Card

2023-12-04 Grocery Store
\tExpenses:Groceries\t50 USD
\tLiabilities:Credit Card"
    (let ((ledger-flymake-be-pedantic nil))
      (should (equal (ledger-accounts-list-in-buffer)
                     '("Expenses:Food"
                       "Expenses:Groceries"
                       "Liabilities:Credit Card"))))
    (let ((ledger-flymake-be-pedantic t))
      (should (equal (ledger-accounts-list-in-buffer)
                     '("Expenses:Food"
                       "Expenses:Groceries"))))))

(defvar ledger-complete--current-time-for-testing)

(ert-deftest ledger-complete/date-month-day ()
  "Test completing an incomplete date with only a month and day.
https://github.com/ledger/ledger-mode/issues/419"
  :tags '(complete regress)
  (let ((ledger-complete--current-time-for-testing ;2024-01-21
         (encode-time 0 0 0 21 1 2024))
        (ledger-default-date-format ledger-iso-date-format)
        ;; TODO: Set up date completion so that it does not require a specific
        ;; completion-style setting.
        (completion-styles '(flex)))
    (ledger-tests-with-temp-file
        "01-19"
      (goto-char (point-max))
      (completion-at-point)
      (should
       (equal (buffer-string)
              "2024-01-19 "))

      (erase-buffer)
      (insert "01-23")
      (completion-at-point)
      (should
       (equal (buffer-string)
              "2023-01-23 ")))))

(ert-deftest ledger-complete/date-day-only ()
  "Test completing an incomplete date with only a day.
https://github.com/ledger/ledger-mode/issues/419"
  :tags '(complete regress)
  (let ((ledger-complete--current-time-for-testing ;2024-01-21
         (encode-time 0 0 0 21 1 2024))
        (ledger-default-date-format ledger-iso-date-format)
        (completion-styles '(flex)))
    (ledger-tests-with-temp-file
        "19"
      (goto-char (point-max))
      (completion-at-point)
      (should
       (equal (buffer-string)
              "2024-01-19 "))

      (erase-buffer)
      (insert "23")
      (completion-at-point)
      (should
       (equal (buffer-string)
              "2023-12-23 ")))))

(provide 'complete-test)

;;; complete-test.el ends here
