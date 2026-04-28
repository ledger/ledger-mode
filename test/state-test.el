;;; state-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-state

;;; Code:
(require 'test-helper)


(ert-deftest ledger-state/test-001 ()
  "Regress test for Bug 1030
http://bugs.ledger-cli.org/show_bug.cgi?id=1030"
  :tags '(state regress)

  (ledger-tests-with-temp-file
"2013/06/09 * 4 Saisons
    Dépense:Alimentation:Restaurant          23,00 €
    Passif:Crédit:BanqueAccord
"
    (forward-line 1)
    (ledger-toggle-current)             ; C-c C-c
    (should
     (equal (buffer-string)
      "2013/06/09 4 Saisons
    Dépense:Alimentation:Restaurant          23,00 €
    * Passif:Crédit:BanqueAccord
" ))
    (ledger-toggle-current)             ; C-c C-c
    (should
     (equal (buffer-string)
      "2013/06/09 * 4 Saisons
    Dépense:Alimentation:Restaurant          23,00 €
    Passif:Crédit:BanqueAccord
" ))))


(ert-deftest ledger-state/test-002 ()
  "General tests for various behaviors of `ledger-toggle-current'."
  :tags '(state)

  (ledger-tests-with-temp-file
      "\
2024-01-01 * Grocery
    Expenses:Groceries    50.00 USD
    Expenses:Household    20.00 USD
    Liabilities:Credit Card
"
    ;; toggling posting while xact is cleared causes other postings to be
    ;; individually cleared
    (forward-line)
    (ledger-toggle-current)
    (should (equal (buffer-string)
                   "\
2024-01-01 Grocery
    Expenses:Groceries    50.00 USD
    * Expenses:Household  20.00 USD
    * Liabilities:Credit Card
"))

    ;; when state char is removed, amounts remain in the original column
    (forward-line)
    (ledger-toggle-current)
    (forward-line)
    (ledger-toggle-current)
    (should (equal (buffer-string)
                   "\
2024-01-01 Grocery
    Expenses:Groceries    50.00 USD
    Expenses:Household    20.00 USD
    Liabilities:Credit Card
"))

    ;; when state char is inserted, amounts remain in the original column
    (forward-line -1)
    (ledger-toggle-current)
    (forward-line -1)
    (ledger-toggle-current)
    (should (equal (buffer-string)
                   "\
2024-01-01 Grocery
    * Expenses:Groceries  50.00 USD
    * Expenses:Household  20.00 USD
    Liabilities:Credit Card
"))

    ;; toggling on xact causes the post state chars to be cleared
    (forward-line -1)
    (ledger-toggle-current)
    (should (equal (buffer-string)
                   "\
2024-01-01 * Grocery
    Expenses:Groceries    50.00 USD
    Expenses:Household    20.00 USD
    Liabilities:Credit Card
"))))


(ert-deftest ledger-state/test-003 ()
  "Regression test for #274
https://github.com/ledger/ledger-mode/issues/274"
  :tags '(state regress)

  ;; deliberately no newline at the end of the file
  (ledger-tests-with-temp-file
   "2011/01/19 Grocery Store
  Expenses:Food:Groceries             $ 44.00 ; hastag: not block
  Assets:Checking"
   (goto-char (point-min))
   (ledger-toggle-current)
   (should
    (equal (buffer-string)
           "2011/01/19 * Grocery Store
  Expenses:Food:Groceries             $ 44.00 ; hastag: not block
  Assets:Checking"))))


(ert-deftest ledger-state/test-004 ()
  "Regression test for #374
https://github.com/ledger/ledger-mode/issues/374"

  (ledger-tests-with-temp-file
      "2024/01/01 Test
    Expenses                                   $4.00
    Assets

;; Some comments.
;; Try hitting `ledger-toggle-current' from this line
"
    (setq ledger-clear-whole-transactions t)
    (goto-char (1- (point-max)))
    (ledger-toggle-current)
    (should
     (equal (buffer-string)
            "2024/01/01 Test
    Expenses                                   $4.00
    Assets

;; Some comments.
;; Try hitting `ledger-toggle-current' from this line
"))))

;;; -------------------------------------------------------------------
;;; Coverage tests for previously uncovered branches
;;; -------------------------------------------------------------------

(ert-deftest ledger-state/transaction-state-cleared ()
  "`ledger-transaction-state' returns 'cleared on a `*'-marked xact.
Covers lines 37-44 in `ledger-transaction-state'."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 * Cleared
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (should (eq 'cleared (ledger-transaction-state)))
    ;; And from a posting line, it backtracks to find the date line
    (forward-line 1)
    (should (eq 'cleared (ledger-transaction-state)))))

(ert-deftest ledger-state/transaction-state-pending ()
  "`ledger-transaction-state' returns 'pending on a `!'-marked xact.
Covers the `(looking-at-p \"!\\\\s-*\")' branch."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 ! Pending
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (should (eq 'pending (ledger-transaction-state)))))

(ert-deftest ledger-state/transaction-state-uncleared ()
  "`ledger-transaction-state' returns nil on an unmarked xact.
Covers the `t' fallthrough branch returning nil."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 Uncleared
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (should (null (ledger-transaction-state)))))

(ert-deftest ledger-state/posting-state-pending ()
  "`ledger-posting-state' returns 'pending on a `!'-marked posting.
Covers the `(looking-at-p \"!\\\\s-*\")' branch in `ledger-posting-state'."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 Test
    ! Assets:Cash    $10
    Expenses:Food
"
    (forward-line 1)
    (should (eq 'pending (ledger-posting-state)))))

(ert-deftest ledger-state/posting-state-cleared ()
  "`ledger-posting-state' returns 'cleared on a `*'-marked posting."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 Test
    * Assets:Cash    $10
    Expenses:Food
"
    (forward-line 1)
    (should (eq 'cleared (ledger-posting-state)))))

(ert-deftest ledger-state/posting-state-from-transaction ()
  "`ledger-posting-state' falls through to transaction-state.
Covers the `(t (ledger-transaction-state))' branch."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 * Test
    Assets:Cash    $10
    Expenses:Food
"
    (forward-line 1)
    (should (eq 'cleared (ledger-posting-state)))))

(ert-deftest ledger-state/state-from-string-comment ()
  "`ledger-state-from-string' returns 'comment for a `;'-prefixed string.
Covers the comment branch (line 77)."
  :tags '(state)
  (should (eq 'comment (ledger-state-from-string ";; comment text")))
  (should (eq 'pending (ledger-state-from-string "!")))
  (should (eq 'cleared (ledger-state-from-string "*")))
  (should (null (ledger-state-from-string "x")))
  ;; nil string returns nil
  (should (null (ledger-state-from-string nil))))

(ert-deftest ledger-state/toggle-shifts-status-to-postings ()
  "Toggling a transaction with marker followed by inline content shifts marker.
Covers line 111: inserting spaces after `*' deletion when `  ' follows."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 * (123) Payee  ; comment
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    ;; Forward line to a posting then toggle - this triggers the
    ;; uncompacting path that shifts marker to postings.
    (forward-line 1)
    (ledger-toggle-current)
    ;; After: transaction is uncleared, postings are individually marked
    ;; except the one we just toggled
    (should (string-match-p "2024/01/01 (123) Payee"
                            (buffer-string)))))

(ert-deftest ledger-state/toggle-current-transaction-cleared-with-style ()
  "`ledger-toggle-current-transaction' with style 'cleared on cleared xact keeps marker.
Function deletes marker then re-inserts when style is 'cleared.
Covers lines 244-248."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 * Test
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (ledger-toggle-current-transaction 'cleared)
    ;; Documented behaviour: marker is deleted then re-inserted.
    (should (string-match-p "^2024/01/01 \\* Test"
                            (buffer-string)))))

(ert-deftest ledger-state/toggle-current-transaction-cleared-no-style ()
  "`ledger-toggle-current-transaction' with no style on cleared xact removes marker.
Covers the `(delete-char 1)' path without re-insertion."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 * Test
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (ledger-toggle-current-transaction)
    (should (string-match-p "^2024/01/01 Test"
                            (buffer-string)))))

(ert-deftest ledger-state/toggle-current-transaction-pending-on-marked ()
  "`ledger-toggle-current-transaction' with no style on pending xact removes marker.
Covers the `(delete-char 1)' branch when state is pending."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 ! Test
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (ledger-toggle-current-transaction)
    (should (string-match-p "^2024/01/01 Test"
                            (buffer-string)))))

(ert-deftest ledger-state/toggle-current-transaction-add-pending ()
  "`ledger-toggle-current-transaction' with style 'pending on uncleared xact adds `!'.
Covers lines 249-252."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 Test
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (ledger-toggle-current-transaction 'pending)
    (should (string-match-p "^2024/01/01 ! Test"
                            (buffer-string)))))

(ert-deftest ledger-state/toggle-current-transaction-add-cleared ()
  "`ledger-toggle-current-transaction' on uncleared xact with no style adds `*'.
Covers lines 253-255."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 Test
    Assets:Cash    $10
    Expenses:Food
"
    (goto-char (point-min))
    (ledger-toggle-current-transaction)
    (should (string-match-p "^2024/01/01 \\* Test"
                            (buffer-string)))))

(ert-deftest ledger-state/toggle-posting-with-double-tab ()
  "Toggling a posting where two tabs follow the account name.
Covers the `(looking-at-p \"\\t\")' branch (line 158)."
  :tags '(state)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2024/01/01 Test\n    Assets:Cash\t\t$10\n    Expenses:Food\n"
      (forward-line 1)
      (ledger-toggle-current)
      (should (string-match-p "\\* Assets:Cash" (buffer-string))))))

(ert-deftest ledger-state/toggle-posting-with-tab-amount ()
  "Toggling a posting whose amount is separated by a tab.
Covers the second `(looking-at-p \" [ \\t]\")' branch (lines 159-160)."
  :tags '(state)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2024/01/01 Test\n\tAssets:Cash\t$10\n\tExpenses:Food\n"
      (forward-line 1)
      (ledger-toggle-current)
      (should (string-match-p "\\* Assets:Cash" (buffer-string))))))

(ert-deftest ledger-state/toggle-all-cleared-compacts ()
  "Toggling postings until all are cleared, then the xact is compacted.
Covers lines 204-212 (xact compaction back to single marker)."
  :tags '(state)
  (ledger-tests-with-temp-file
      "2024/01/01 Test
    Assets:Cash    $10
    Expenses:Food   $-10
"
    (forward-line 1)
    ;; Toggle first posting to cleared
    (ledger-toggle-current)
    ;; Toggle second posting to cleared - this should compact xact
    (forward-line 1)
    (ledger-toggle-current)
    ;; All postings cleared, xact should be compacted with single `*'
    (should (string-match-p "^2024/01/01 \\* Test"
                            (buffer-string)))
    ;; Postings should not have individual markers
    (should-not (string-match-p "    \\* " (buffer-string)))))

(ert-deftest ledger-state/toggle-with-tab-after-account-compaction ()
  "Compaction path when account is followed by a tab (line 207-208).
Covers `(looking-at-p \"\\t\")' branch in compaction excursion."
  :tags '(state)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2024/01/01 Test\n    Assets:Cash\t$10\n    Expenses:Food\t$-10\n"
      (forward-line 1)
      (ledger-toggle-current)
      (forward-line 1)
      (ledger-toggle-current)
      ;; All cleared, so compaction kicks in
      (should (string-match-p "^2024/01/01 \\* Test"
                              (buffer-string))))))

(ert-deftest ledger-state/compaction-double-tab-after-payee ()
  "Compaction where two tabs follow the payee on the date line.
Covers line 208: `(delete-char 1)' for tab in compaction (the second
tab is detected by `looking-at-p \"\\t\"' after the first tab is matched)."
  :tags '(state)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2024/01/01 Test\t\t; comment\n    Assets:Cash    $10\n    Expenses:Food   $-10\n"
      (forward-line 1)
      (ledger-toggle-current)
      (forward-line 1)
      (ledger-toggle-current)
      (should (string-match-p "^2024/01/01 \\* Test"
                              (buffer-string))))))

(ert-deftest ledger-state/compaction-double-space-then-space ()
  "Compaction where after match we see ` [ \\t]'.
Covers line 210: `(delete-char 2)' in compaction.
Three spaces in a row mean: search matches the first two-space pair, point
is on the third space, then `looking-at-p \" [ \\t]\"' is true."
  :tags '(state)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2024/01/01 Test    ; comment\n    Assets:Cash    $10\n    Expenses:Food   $-10\n"
      (forward-line 1)
      (ledger-toggle-current)
      (forward-line 1)
      (ledger-toggle-current)
      (should (string-match-p "^2024/01/01 \\* Test"
                              (buffer-string))))))

(ert-deftest ledger-state/compaction-double-space-then-non-space ()
  "Compaction where after match we see exactly one space (line 212).
Three-space pattern: search matches `  ' (two-space), point on 3rd space,
the next char is non-space (the `;'), so single-space branch fires."
  :tags '(state)
  (let ((ledger-post-auto-align nil))
    (ledger-tests-with-temp-file
        "2024/01/01 Test   ; comment\n    Assets:Cash    $10\n    Expenses:Food   $-10\n"
      (forward-line 1)
      (ledger-toggle-current)
      (forward-line 1)
      (ledger-toggle-current)
      (should (string-match-p "^2024/01/01 \\* Test"
                              (buffer-string))))))

(provide 'state-test)

;;; state-test.el ends here
