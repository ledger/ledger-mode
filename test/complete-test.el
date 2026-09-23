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

(ert-deftest ledger-complete/test-complete-virtual-account-both-parens ()
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    (blo)"
    (goto-char (1- (point-max)))
    (call-interactively 'completion-at-point)
    (should
     (equal (buffer-string)
            "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    (bloop)"))))

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

(ert-deftest ledger-complete/test-complete-payee-point-inside ()
  "Completion boundaries are correct for payees."
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "payee Foo Bar

2019/06/28 Foo Bar"
    (goto-char (point-max))
    (backward-word 1)
    (let ((inhibit-interaction t))       ;require a unique match
      (completion-at-point))
    (should
     (equal (buffer-string)
            "payee Foo Bar

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
2019/01/01 Company
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
    (let ((completion-cycle-threshold t)
          (ledger-complete-in-steps t))
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
  (ledger-tests-with-temp-file
      "2019/06/28 Foobar
\tExpenses\t11.99 CAD
\tEx\t-11.99 CAD"
    (let ((ledger-post-auto-align nil))
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
         (encode-time 0 0 0 21 1 2024)))
    (ledger-tests-with-temp-file
        "01-19"
      (setq ledger-default-date-format ledger-iso-date-format)
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
         (encode-time 0 0 0 21 1 2024)))
    (ledger-tests-with-temp-file
        "19"
      (setq ledger-default-date-format ledger-iso-date-format)
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
              "2023-12-23 "))

      (erase-buffer)
      (insert "23")
      (backward-char)
      ;; completion uses whole day number, not just the part before point
      (completion-at-point)
      (should
       (equal (buffer-string)
              "2023-12-23 ")))))

(ert-deftest ledger-complete/account-complete-error ()
  "Regression test for https://github.com/ledger/ledger-mode/issues/443."
  :tags '(complete regress)
  (let ((ledger-complete-in-steps t))
    (ledger-tests-with-temp-file
        "\
;
2025/01/06 * Cinema
    Expenses:Cinema                           €18.00
    Cash"
      (goto-char (point-min))
      (end-of-line)
      (insert " ")
      (let ((completion-in-region-function
             (lambda (start end collection predicate)
               (should (null (all-completions (buffer-substring start end)
                                              collection predicate))))))
        (call-interactively 'completion-at-point)))))

(ert-deftest ledger-complete/complete-txn-comment ()
  "Completion over other comments in buffer.
Regression test for https://github.com/ledger/ledger-mode/pull/455."
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "\
; file comment

2025/12/07 Grocery
    Expenses:Groceries  $10
    ; transaction comment
    Liabilities:Credit Card

2025/12/08 Grocery
    ; tra"
    (goto-char (point-max))
    (completion-at-point)
    (should (equal (buffer-substring (line-beginning-position) (line-end-position))
                   "    ; transaction comment"))))


(ert-deftest ledger-complete/complete-txn-comment-point-inside ()
  "Completion uses correct boundaries for transaction comments."
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "\
; file comment

2025/12/07 Grocery
    Expenses:Groceries  $10
    ; transaction comment
    Liabilities:Credit Card"
    (search-forward "transaction comment")
    (backward-word 1)
    (completion-at-point)
    (should (equal (buffer-substring (line-beginning-position) (line-end-position))
                   "    ; transaction comment"))))


;;; -------------------------------------------------------------------
;;; Coverage tests for previously uncovered branches
;;; -------------------------------------------------------------------

(ert-deftest ledger-complete/payees-list-from-file ()
  "`ledger-payees-list' should consult `ledger-payees-file' when set.
Covers the file-based branch in `ledger-payees-list'."
  :tags '(complete)
  (let ((payee-file (make-temp-file "ledger-test-payees-"))
        (ledger-init-file-name nil)
        (ledger-environment-alist nil))
    (unwind-protect
        (progn
          (with-temp-file payee-file
            ;; Leading blank line so that Alpha's match doesn't begin at
            ;; point-min, otherwise `ledger-payees-in-buffer' skips it.
            (insert "\n")
            (insert "payee Alpha\npayee Bravo\n")
            (insert "2025/01/01 Charlie\n    Expenses:Food  $1\n    Assets:Cash\n"))
          (with-temp-buffer
            (ledger-mode)
            (let ((ledger-payees-file payee-file))
              (let ((payees (ledger-payees-list)))
                (should (member "Alpha" payees))
                (should (member "Bravo" payees))
                (should (member "Charlie" payees))))))
      (delete-file payee-file))))

(ert-deftest ledger-complete/accounts-list-from-file ()
  "`ledger-accounts-list' should consult `ledger-accounts-file' when set.
Covers the file-based branch in `ledger-accounts-list'."
  :tags '(complete)
  (let ((accts-file (make-temp-file "ledger-test-accts-"))
        (ledger-init-file-name nil)
        (ledger-environment-alist nil))
    (unwind-protect
        (progn
          (with-temp-file accts-file
            (insert "account Assets:Bank\n")
            (insert "account Expenses:Food\n"))
          (with-temp-buffer
            (ledger-mode)
            (let ((ledger-accounts-file accts-file))
              (let ((accounts (ledger-accounts-list)))
                (should (member "Assets:Bank" accounts))
                (should (member "Expenses:Food" accounts))))))
      (delete-file accts-file))))

(ert-deftest ledger-complete/account-directive-data-without-space ()
  "Account directive sub-lines without spaces are stored with nil cdr.
Covers the `(push (cons d nil) data)' branch in `ledger-accounts-in-buffer'."
  :tags '(complete)
  (let ((ledger-init-file-name nil)
        (ledger-environment-alist nil))
    (with-temp-buffer
      (ledger-mode)
      (insert "account Assets:Bank\n")
      (insert "    default\n")
      (insert "    note something\n")
      (let* ((accounts (ledger-accounts-in-buffer))
             (entry (assoc "Assets:Bank" accounts))
             (data (cdr entry)))
        (should entry)
        ;; "default" has no space, stored as (cons "default" nil)
        (should (assoc "default" data))
        (should (null (cdr (assoc "default" data))))
        ;; "note something" has a space, stored as ("note" . "something")
        (should (equal (cdr (assoc "note" data)) "something"))))))

(ert-deftest ledger-complete/account-next-steps-multi-prefix ()
  "Test `ledger-complete-account-next-steps' descending into a multi-element prefix.
Covers the inner while-loop traversal of nested account elements."
  :tags '(complete)
  (let ((ledger-init-file-name nil)
        (ledger-environment-alist nil))
    (with-temp-buffer
      (ledger-mode)
      (insert "account Assets:Bank:Checking\n")
      (insert "account Assets:Bank:Savings\n")
      (insert "account Assets:Cash\n")
      (insert "account Expenses:Food\n")
      ;; Insert prefix and ask for next steps starting at "Assets:Bank:"
      (let ((p1 (point-max)))
        (insert "Assets:Bank:")
        (let* ((p2 (point-max))
               (steps (ledger-complete-account-next-steps p1 p2)))
          (should (member "Assets:Bank:Checking" steps))
          (should (member "Assets:Bank:Savings" steps)))))))

(ert-deftest ledger-complete/account-next-steps-unknown-prefix ()
  "When a prefix element is unknown, descent stops and returns nil.
Covers the `(setq root nil elements nil)' arm."
  :tags '(complete)
  (let ((ledger-init-file-name nil)
        (ledger-environment-alist nil))
    (with-temp-buffer
      (ledger-mode)
      (insert "account Assets:Bank:Checking\n")
      (let ((p1 (point-max)))
        (insert "Assets:Nope:")
        (let* ((p2 (point-max))
               (steps (ledger-complete-account-next-steps p1 p2)))
          (should (null steps)))))))

(ert-deftest ledger-complete/complete-date-month-day-current-year ()
  "Completing a date with month/day in the past of current year.
Returns this year's date when not already past."
  :tags '(complete)
  (ledger-tests-with-temp-file "03-15"
    (let ((ledger-complete--current-time-for-testing
           (encode-time 0 0 0 21 6 2024)) ; 2024-06-21
          (ledger-default-date-format ledger-iso-date-format))
      (goto-char (point-max))
      (completion-at-point)
      (should (equal (buffer-string) "2024-03-15 ")))))

(ert-deftest ledger-complete/complete-date-day-only-january ()
  "Completing a day-only when current month is January falls back to Dec last year.
Covers `last-month'/`last-year' branches in `ledger-complete-date'."
  :tags '(complete)
  (ledger-tests-with-temp-file "10"
    (let ((ledger-complete--current-time-for-testing
           (encode-time 0 0 0 5 1 2024)) ; 2024-01-05
          (ledger-default-date-format ledger-iso-date-format))
      (goto-char (point-max))
      (completion-at-point)
      ;; January has past day 10? No - current is Jan 5. So past day 10 is in
      ;; December of last year (2023-12-10).
      (should (equal (buffer-string) "2023-12-10 ")))))

(defun ledger-complete-test--call-capf-table ()
  "Return (start end candidate-list) for the active completion-at-point function."
  (let ((capf (run-hook-with-args-until-success 'completion-at-point-functions)))
    (when capf
      (let* ((start (nth 0 capf))
             (end (nth 1 capf))
             (coll (nth 2 capf)))
        (list start end (all-completions "" coll))))))

(ert-deftest ledger-complete/effective-date-cond-branch ()
  "`ledger-complete-at-point' selects effective-date branch when YYYY-MM-DD=MM-DD is typed.
Covers lines 302-306 in `ledger-complete-at-point' that set
`start' and `collection' for the effective-date branch."
  :tags '(complete)
  (ledger-tests-with-temp-file "2024-06-15=06-20"
    (let ((ledger-default-date-format ledger-iso-date-format))
      (goto-char (point-max))
      (let ((res (ledger-complete-test--call-capf-table)))
        (should res)
        ;; start should be at line-beginning-position (1)
        (should (= 1 (nth 0 res)))
        ;; the candidate should be the resolved effective date with trailing space
        (should (member "2024-06-20 " (nth 2 res)))))))

(ert-deftest ledger-complete/effective-date-direct-call ()
  "Direct call to `ledger-complete-effective-date' produces correct candidates.
Covers all body lines of `ledger-complete-effective-date'."
  :tags '(complete)
  (let ((ledger-default-date-format ledger-iso-date-format))
    (let ((coll (ledger-complete-effective-date "2024" "06" "15" "06" "20" t)))
      (should (functionp coll))
      ;; Metadata branch
      (should (equal (funcall coll "" nil 'metadata)
                     '(metadata (category . ledger-date))))
      ;; Action branch returning candidate list
      (should (member "2024-06-20 " (funcall coll "" nil t))))))

(ert-deftest ledger-complete/effective-date-day-only ()
  "Effective date with only a day completes to same month next or current year.
Covers the day-only branch in `ledger-complete-effective-date'."
  :tags '(complete)
  (let* ((ledger-default-date-format ledger-iso-date-format)
         (coll (ledger-complete-effective-date "2024" "06" "15" nil "20" nil)))
    ;; day=20 >= tx-day=15 in tx-month, so it picks 2024-06-20
    (should (member "2024-06-20" (funcall coll "" nil t)))))

(ert-deftest ledger-complete/effective-date-wrap-year ()
  "Effective date in December wraps to next year.
Covers `next-year'/`next-month-year' selection."
  :tags '(complete)
  (let* ((ledger-default-date-format ledger-iso-date-format)
         (coll (ledger-complete-effective-date "2024" "12" "15" "01" "05" nil)))
    ;; Day 5 of January after a 2024-12-15 transaction wraps to 2025-01-05
    (should (member "2025-01-05" (funcall coll "" nil t)))))

(ert-deftest ledger-complete/complete-date-uses-current-time ()
  "Direct call to `ledger-complete-date' uses (current-time) when var is nil.
Covers the `(current-time)' branch on line 220."
  :tags '(complete)
  ;; Ensure the test variable is unset so the (current-time) path is taken.
  (let ((ledger-complete--current-time-for-testing nil))
    (let ((coll (ledger-complete-date "01" "15" nil)))
      (should (functionp coll))
      ;; Should produce a single candidate (don't care exact date).
      (should (= 1 (length (funcall coll "" nil t)))))))

(ert-deftest ledger-complete/complete-date-collection-callable ()
  "The completion table from `ledger-complete-date' supports metadata.
Covers the metadata branch of the returned closure of `ledger-complete-date'."
  :tags '(complete)
  (let* ((ledger-complete--current-time-for-testing
          (encode-time 0 0 0 21 6 2024))
         (coll (ledger-complete-date "06" "15" t)))
    (should (functionp coll))
    (should (equal (funcall coll "" nil 'metadata)
                   '(metadata (category . ledger-date))))))

(ert-deftest ledger-complete/fully-complete-xact-error ()
  "`ledger-fully-complete-xact' signals user-error when not on a transaction.
Covers the `user-error' branch."
  :tags '(complete)
  (ledger-tests-with-temp-file
      "; just a comment\n"
    (goto-char (point-min))
    (end-of-line)
    (should-error (ledger-fully-complete-xact) :type 'user-error)))

(ert-deftest ledger-complete/exact-payee-present-in-buffer ()
  "Completion may include the exact string at point if it is present elsewhere."
  :tags '(complete regress)
  (ledger-tests-with-temp-file "\
2026-01-01 Grocery Store
    Expenses:Groceries  $10
    Assets:Cash

2026-01-02 Grocery Store 2
    Expenses:Groceries  $10
    Assets:Cash

2026-01-03 Grocery Stor
    Expenses:Groceries  $10
    Assets:Cash
"
    (goto-char (point-max))
    (forward-line -3)
    (end-of-line)
    ;; uniquely completes to longest common prefix: "Grocery Store"
    (let ((inhibit-interaction t))
      (completion-at-point))
    (should (equal
             (buffer-substring-no-properties (line-beginning-position) (line-end-position))
             "2026-01-03 Grocery Store"))
    ;; Since this is a valid payee somewhere else in the buffer, no need to
    ;; change it.
    (let ((inhibit-interaction t))
      (completion-at-point))
    (should (equal
             (buffer-substring-no-properties (line-beginning-position) (line-end-position))
             "2026-01-03 Grocery Store"))))

(ert-deftest ledger-complete/exact-account-present-in-buffer ()
  "Completion may include the exact string at point if it is present elsewhere."
  :tags '(complete regress)
  (ledger-tests-with-temp-file "\
2026-01-01 Grocery Store
    Expenses:Groceries  $10
    Assets:Cash

2026-01-02 Grocery Store 2
    Expenses:Groceries:Snacks  $10
    Assets:Cash

2026-01-03 Grocery Stor
    Expenses:Groc  $10
    Assets:Cash
"
    (let ((ledger-post-auto-align nil))
      (goto-char (point-max))
      (forward-line -2)
      (forward-word 2)
      ;; uniquely completes to longest common prefix: "Expenses:Groceries"
      (let ((inhibit-interaction t))
        (completion-at-point))
      (should (equal
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               "    Expenses:Groceries  $10"))
      ;; Since this is a valid account somewhere else in the buffer, no need to
      ;; change it.
      (let ((inhibit-interaction t))
        (completion-at-point))
      (should (equal
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               "    Expenses:Groceries  $10")))))

(provide 'complete-test)

;;; complete-test.el ends here
