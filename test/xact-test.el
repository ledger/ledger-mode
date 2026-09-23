;;; xact-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-xact

;;; Code:
(require 'test-helper)


(ert-deftest ledger-xact/test-001 ()
  "Regress test for Bug 952+936+183
http://bugs.ledger-cli.org/show_bug.cgi?id=952
http://bugs.ledger-cli.org/show_bug.cgi?id=936
http://bugs.ledger-cli.org/show_bug.cgi?id=183"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
      "2013/05/01 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (goto-char (point-max))              ; end-of-buffer
    (ledger-add-transaction "2013/05/02 foo")
    (should
     (equal (buffer-string)
            "2013/05/01 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
" ))))


(ert-deftest ledger-xact/test-002 ()
  "Regress test for Bug 526
http://bugs.ledger-cli.org/show_bug.cgi?id=526"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
      "2013/05/01 foo
    Expenses:Foo                             10,00 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             10,00 €
    Assets:Bar
"
    (goto-char (point-max))              ; end-of-buffer
    (ledger-add-transaction "2013/05/02 foo 16,02")
    (should
     (equal (buffer-string)
            "2013/05/01 foo
    Expenses:Foo                             10,00 €
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                               16,02 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             10,00 €
    Assets:Bar
"))))


(ert-deftest ledger-xact/test-003 ()
  "Regression test for #307
https://github.com/ledger/ledger-mode/issues/307"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
      "2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (goto-char (point-max))              ; end-of-buffer
    (ledger-add-transaction "2013/05/01 foo")
    (should
     (equal (buffer-string)
            "2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"))))

(ert-deftest ledger-xact/test-004 ()
  "Add xact following all other xacts."
  :tags '(xact)

  (ledger-tests-with-temp-file
      "\
2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"

    (ledger-add-transaction "2013/05/04 foo")
    (should
     (equal (buffer-string)
            "\
2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/04 foo
    Expenses:Foo                              $10.00
    Assets:Bar
"))))

;;; ----------------------------------------------------------------
;;; ledger-highlight--before-revert (line 75)
;;; ----------------------------------------------------------------

(ert-deftest ledger-xact/highlight-before-revert-deletes-overlay ()
  "`ledger-highlight--before-revert' deletes the highlight overlay."
  :tags '(xact)
  (ledger-tests-with-temp-file demo-ledger
    (forward-line 1)
    (ledger-highlight-xact-under-point)
    (let ((ovl ledger-xact-highlight-overlay))
      (should ovl)
      (should (overlay-buffer ovl))
      (ledger-highlight--before-revert)
      (should-not (overlay-buffer ovl)))))


;;; ----------------------------------------------------------------
;;; ledger-xact-context / ledger-xact-payee / ledger-xact-date
;;; (lines 79-94)
;;; ----------------------------------------------------------------

(ert-deftest ledger-xact/xact-payee-from-posting ()
  "When point is on a posting line, the payee of the parent xact is returned."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2010/12/01 * Checking balance
  Assets:Checking                   $1,000.00
  Equity:Opening Balances
"
    (forward-line 2) ; sit on second posting
    (should (string= "Checking balance" (ledger-xact-payee)))
    (should (string= "2010/12/01" (ledger-xact-date)))))

(ert-deftest ledger-xact/xact-payee-on-xact-line ()
  "When point is on an xact line, the same payee is returned."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2010/12/01 * Checking balance
  Assets:Checking                   $1,000.00
  Equity:Opening Balances
"
    (should (string= "Checking balance" (ledger-xact-payee)))
    (should (string= "2010/12/01" (ledger-xact-date)))))

(ert-deftest ledger-xact/xact-payee-no-context ()
  "Outside a transaction, the helpers return nil."
  :tags '(xact)
  (ledger-tests-with-temp-file ""
    (should-not (ledger-xact-payee))
    (should-not (ledger-xact-date))
    (should-not (ledger-xact-context))))


;;; ----------------------------------------------------------------
;;; ledger-xact-iterate-transactions, year directive (lines 121, 132)
;;; ----------------------------------------------------------------

(ert-deftest ledger-xact/iterate-transactions-year-directive ()
  "A `Y' directive updates the current year used for iteration (line 121)."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "Y 2020

2019/03/15 Foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (let ((count 0))
      (ledger-xact-iterate-transactions
       (lambda (_start _date _state _payee)
         (setq count (1+ count))))
      ;; The Y directive at the top is silently consumed and the dated
      ;; transaction is reported.
      (should (= 1 count)))))

(ert-deftest ledger-xact/iterate-transactions-explicit-year ()
  "An explicit year on the date is honoured (line 132)."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2019/03/15 Foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (let (collected)
      (ledger-xact-iterate-transactions
       (lambda (_start date _state _payee)
         (push (decode-time date) collected)))
      (should (= 1 (length collected)))
      ;; Year reported should reflect the year on the date line.
      (should (= 2019 (nth 5 (car collected)))))))


;;; ----------------------------------------------------------------
;;; ledger-copy-transaction-at-point (lines 144-163)
;;; ----------------------------------------------------------------

(ert-deftest ledger-xact/copy-transaction-at-point ()
  "Copy current transaction to a new date."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2024/01/01 Foo
    Expenses:Foo                            $10.00
    Assets:Bar

2024/01/05 Bar
    Expenses:Bar                            $20.00
    Assets:Bar
"
    ;; Place point on the first transaction.
    (goto-char (point-min))
    (ledger-copy-transaction-at-point "2024/01/03")
    ;; The buffer should now contain three xacts ordered chronologically.
    (let ((str (buffer-string)))
      (should (string-match-p "2024/01/01 Foo" str))
      (should (string-match-p "2024/01/03 Foo" str))
      (should (string-match-p "2024/01/05 Bar" str)))))

(ert-deftest ledger-xact/copy-transaction-blank-line-after ()
  "When `ledger-copy-transaction-insert-blank-line-after' is set and not at
end-of-buffer, the inserted xact gets a trailing blank line (line 151-152)."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2024/01/01 Foo
    Expenses:Foo                            $10.00
    Assets:Bar

2024/01/05 Bar
    Expenses:Bar                            $20.00
    Assets:Bar
"
    (let ((ledger-copy-transaction-insert-blank-line-after t))
      (goto-char (point-min))
      (ledger-copy-transaction-at-point "2024/01/03"))
    (should (string-match-p "2024/01/03 Foo" (buffer-string)))))

(ert-deftest ledger-xact/copy-transaction-no-amount ()
  "Copy transaction whose first posting has no amount.
Hits `ledger-next-account' fallback (line 163)."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2024/01/01 Foo
    Assets:Bar
    Expenses:Foo

2024/01/05 Bar
    Expenses:Bar                            $20.00
    Assets:Bar
"
    (goto-char (point-min))
    (ledger-copy-transaction-at-point "2024/01/03")
    (should (string-match-p "2024/01/03 Foo" (buffer-string)))))

(ert-deftest ledger-xact/copy-transaction-interactive-form ()
  "Interactive call reads a date via `ledger-read-date'."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2024/01/01 Foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (cl-letf (((symbol-function 'ledger-read-date)
               (lambda (_p) "2024/01/03")))
      (goto-char (point-min))
      (call-interactively #'ledger-copy-transaction-at-point))
    (should (string-match-p "2024/01/03 Foo" (buffer-string)))))


;;; ----------------------------------------------------------------
;;; ledger-read-transaction (lines 177-180)
;;; ----------------------------------------------------------------

(ert-deftest ledger-xact/read-transaction-with-text ()
  "When prompt-for-text is on, the xact text is appended to the date."
  :tags '(xact)
  (let ((ledger-add-transaction-prompt-for-text t))
    (cl-letf (((symbol-function 'ledger-read-date)
               (lambda (_p) "2024/01/03"))
              ((symbol-function 'read-string)
               (lambda (&rest _args) "Acme $5")))
      (should (string= "2024/01/03 Acme $5"
                       (ledger-read-transaction))))))

(ert-deftest ledger-xact/read-transaction-no-text ()
  "When prompt-for-text is nil, the result is just the date plus a space."
  :tags '(xact)
  (let ((ledger-add-transaction-prompt-for-text nil))
    (cl-letf (((symbol-function 'ledger-read-date)
               (lambda (_p) "2024/01/03")))
      (should (string= "2024/01/03 "
                       (ledger-read-transaction))))))


;;; ----------------------------------------------------------------
;;; ledger-add-transaction interactive form (lines 198-224)
;;; ----------------------------------------------------------------

(ert-deftest ledger-xact/add-transaction-interactive-no-text ()
  "Interactive add-transaction with only a date inserts the date and a space."
  :tags '(xact)
  (ledger-tests-with-temp-file ""
    (cl-letf (((symbol-function 'ledger-read-date)
               (lambda (_p) "2024/01/05"))
              ((symbol-function 'read-string)
               (lambda (&rest _args) "")))
      (let ((ledger-add-transaction-prompt-for-text nil))
        (call-interactively #'ledger-add-transaction)))
    ;; The single-arg branch (line 223-224): "<date> " then a newline.
    (should (string-match-p "\\`2024/01/05 " (buffer-string)))))

(ert-deftest ledger-xact/add-transaction-at-point ()
  "INSERT-AT-POINT non-nil bypasses the find-slot branch (line 204 unless).
With a single token (just the date), no ledger xact call is needed and
the date is inserted in place at point."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2024/01/01 Foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (goto-char (point-max))
    (ledger-add-transaction "2024/01/02" t)
    (should (string-match-p "2024/01/02 " (buffer-string)))))

(ert-deftest ledger-xact/add-transaction-records-last-date ()
  "Adding a transaction stores the parsed date in `ledger-add-transaction-last-date'.
A single-token transaction text avoids invoking ledger xact, so this
test is hermetic."
  :tags '(xact)
  (ledger-tests-with-temp-file
      "2024/01/01 Foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (let ((ledger-add-transaction-last-date nil))
      (ledger-add-transaction "2024/01/02")
      (should ledger-add-transaction-last-date)
      (should (= 2024
                 (nth 5 (decode-time ledger-add-transaction-last-date)))))))

(ert-deftest ledger-xact/add-transaction-unparseable-date ()
  "Unparseable date causes the `(or parsed-date date)' clause to fall through
to the date string itself (line 210)."
  :tags '(xact)
  (ledger-tests-with-temp-file ""
    ;; `ledger-xact-find-slot' tolerates non-time arguments because it only
    ;; uses them when iterating against existing transactions.  An empty
    ;; buffer means it is a no-op.
    (let ((ledger-add-transaction-last-date 'sentinel))
      (ledger-add-transaction "garbage")
      ;; parse failed -> stored as nil
      (should (null ledger-add-transaction-last-date))
      (should (string-match-p "garbage " (buffer-string))))))


(ert-deftest ledger-xact/test-add-transaction-word-splitting ()
  "Add xact with quoted/escaped arguments."
  :tags '(xact)

  (ledger-tests-with-temp-file
      "\
2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo bar
    Expenses:Bar                            $10.00
    Assets:Foo

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (save-buffer)

    ;; arguments are normally word-split
    (ledger-add-transaction "2013/05/04 foo bar")
    (should
     (equal (thing-at-point 'paragraph)
            "
2013/05/04 foo
    Assets:Bar
    Assets:Bar
"))
    (revert-buffer t t)

    ;; word splitting can be inhibited by quoting...
    (ledger-add-transaction "2013/05/04 \"foo bar\"")
    (should
     (equal (thing-at-point 'paragraph)
            "
2013/05/04 foo bar
    Expenses:Bar                              $10.00
    Assets:Foo
"))
    (revert-buffer t t)

    ;; ...or backslash-escaping
    (ledger-add-transaction "2013/05/04 foo\\ bar")
    (should
     (equal (thing-at-point 'paragraph)
            "
2013/05/04 foo bar
    Expenses:Bar                              $10.00
    Assets:Foo
"))))

(provide 'xact-test)

;;; xact-test.el ends here
