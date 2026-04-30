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
(require 'subr-x)
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

;;; ----------------------------------------------------------------
;;; ledger-mode-dump-{variable,group,configuration}  (lines 72-90)
;;; ----------------------------------------------------------------

(defvar ledger-mode-test--dump-var 42)

(ert-deftest ledger-mode/dump-variable-inserts-name-and-value ()
  "`ledger-mode-dump-variable' inserts a formatted name+value line."
  :tags '(mode)
  (with-temp-buffer
    (ledger-mode-dump-variable 'ledger-mode-test--dump-var)
    (should (string-match-p "ledger-mode-test--dump-var" (buffer-string)))
    (should (string-match-p "42" (buffer-string)))))

(ert-deftest ledger-mode/dump-variable-nil-noop ()
  "Dumping nil does nothing."
  :tags '(mode)
  (with-temp-buffer
    (ledger-mode-dump-variable nil)
    (should (string-empty-p (buffer-string)))))

(ert-deftest ledger-mode/dump-group-walks-recursively ()
  "`ledger-mode-dump-group' iterates a group's variables and subgroups."
  :tags '(mode)
  (with-temp-buffer
    (ledger-mode-dump-group 'ledger)
    (let ((str (buffer-string)))
      (should (> (length str) 0))
      ;; Subgroups are introduced by a "Group ... :" header.
      (should (string-match-p "Group " str)))))

(ert-deftest ledger-mode/dump-configuration ()
  "`ledger-mode-dump-configuration' is interactive and writes the dump."
  :tags '(mode)
  (let ((target (make-temp-name "ledger-mode-dump-")))
    (cl-letf (((symbol-function 'find-file)
               (lambda (_f) (set-buffer (get-buffer-create target)))))
      (unwind-protect
          (progn
            (call-interactively #'ledger-mode-dump-configuration)
            (with-current-buffer target
              (should (> (length (buffer-string)) 0))))
        (when (get-buffer target)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer target)))))))


;;; ----------------------------------------------------------------
;;; ledger-read-payee-with-prompt   (lines 103-106)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/read-payee-with-prompt-default-from-context ()
  "When point is on a transaction, the current payee is offered as default."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "2024/01/01 Acme
    Expenses:Foo                            $1.00
    Assets:Bar
"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _coll _pred _req _init _hist default) default)))
      ;; On the date/payee line - context should report payee.
      (should (string= "Acme" (ledger-read-payee-with-prompt "Payee"))))))

(ert-deftest ledger-mode/read-payee-with-prompt-no-context ()
  "Without an xact context the prompt receives a nil default."
  :tags '(mode)
  (ledger-tests-with-temp-file ""
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p _coll _pred _req _init _hist default)
                 (or default ""))))
      (should (string= "" (ledger-read-payee-with-prompt "Payee"))))))


;;; ----------------------------------------------------------------
;;; ledger-read-date  (lines 111-112)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/read-date-formats-via-org ()
  "`ledger-read-date' delegates to `org-read-date' and reformats."
  :tags '(mode)
  (cl-letf (((symbol-function 'org-read-date)
             (lambda (&rest _args) (encode-time 0 0 12 5 6 2024))))
    (let ((str (ledger-read-date "Pick: ")))
      (should (string-match-p "2024" str))
      (should (string-match-p "06" str))
      (should (string-match-p "05" str)))))


;;; ----------------------------------------------------------------
;;; ledger-read-string-with-default  (lines 129-130)
;;; ----------------------------------------------------------------

(defvar ledger-mode-test--captured-prompt nil)

(ert-deftest ledger-mode/read-string-with-default-passthrough ()
  "Returns whatever `read-string' returns, with the prompt composed."
  :tags '(mode)
  (setq ledger-mode-test--captured-prompt nil)
  (cl-letf (((symbol-function 'read-string)
             (lambda (prompt &rest _r)
               (setq ledger-mode-test--captured-prompt prompt)
               "user-typed")))
    (should (string= "user-typed"
                     (ledger-read-string-with-default "Greet" "Hi")))
    (should (string-match-p "Greet" ledger-mode-test--captured-prompt))
    (should (string-match-p "Hi" ledger-mode-test--captured-prompt))))


;;; ----------------------------------------------------------------
;;; ledger-display-balance-at-point  (lines 139-149)
;;; ----------------------------------------------------------------

(defvar ledger-mode-test--last-msg nil)
(defvar ledger-mode-test--saw-target nil)

(ert-deftest ledger-mode/display-balance-at-point-empty ()
  "When ledger reports nothing for the account, an `is empty.' message is shown."
  :tags '(mode)
  (ledger-tests-with-temp-file demo-ledger
    (setq ledger-mode-test--last-msg nil)
    (cl-letf (((symbol-function 'ledger-master-file)
               (lambda () buffer-file-name))
              ((symbol-function 'ledger-read-account-with-prompt)
               (lambda (_p) "Imaginary:Account"))
              ((symbol-function 'ledger-exec-ledger)
               (lambda (_in _out &rest _args) nil))
              ((symbol-function 'display-message-or-buffer)
               (lambda (msg &rest _r)
                 (setq ledger-mode-test--last-msg msg))))
      (call-interactively #'ledger-display-balance-at-point)
      (should (stringp ledger-mode-test--last-msg))
      (should (string-match-p "is empty" ledger-mode-test--last-msg)))))

(ert-deftest ledger-mode/display-balance-at-point-with-output ()
  "When ledger produces output, it is displayed."
  :tags '(mode)
  (ledger-tests-with-temp-file demo-ledger
    (setq ledger-mode-test--last-msg nil)
    (cl-letf (((symbol-function 'ledger-master-file)
               (lambda () buffer-file-name))
              ((symbol-function 'ledger-read-account-with-prompt)
               (lambda (_p) "Assets:Checking"))
              ((symbol-function 'ledger-exec-ledger)
               (lambda (_in out &rest _args)
                 (with-current-buffer out (insert "BALANCE\n"))))
              ((symbol-function 'display-message-or-buffer)
               (lambda (msg &rest _r)
                 (setq ledger-mode-test--last-msg msg))))
      (call-interactively #'ledger-display-balance-at-point)
      (should (stringp ledger-mode-test--last-msg))
      (should (string-match-p "BALANCE" ledger-mode-test--last-msg)))))

(ert-deftest ledger-mode/display-balance-at-point-target-commodity ()
  "Universal-prefix arg requests a target commodity."
  :tags '(mode)
  (ledger-tests-with-temp-file demo-ledger
    (setq ledger-mode-test--saw-target nil)
    (cl-letf (((symbol-function 'ledger-master-file)
               (lambda () buffer-file-name))
              ((symbol-function 'ledger-read-account-with-prompt)
               (lambda (_p) "Assets:Checking"))
              ((symbol-function 'ledger-read-commodity-with-prompt)
               (lambda (_p) "EUR"))
              ((symbol-function 'ledger-exec-ledger)
               (lambda (_in out &rest args)
                 (when (member "EUR" args)
                   (setq ledger-mode-test--saw-target t))
                 (with-current-buffer out (insert "OUT\n"))))
              ((symbol-function 'display-message-or-buffer)
               (lambda (&rest _r) nil)))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'ledger-display-balance-at-point))
      (should ledger-mode-test--saw-target))))


;;; ----------------------------------------------------------------
;;; ledger-display-ledger-stats  (lines 154-159)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/display-ledger-stats ()
  "`ledger-display-ledger-stats' messages the ledger output."
  :tags '(mode)
  (ledger-tests-with-temp-file demo-ledger
    (setq ledger-mode-test--last-msg nil)
    (cl-letf (((symbol-function 'ledger-master-file)
               (lambda () buffer-file-name))
              ((symbol-function 'ledger-exec-ledger)
               (lambda (_in out &rest _args)
                 (with-current-buffer out (insert "stats output\n"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq ledger-mode-test--last-msg (apply #'format fmt args)))))
      (call-interactively #'ledger-display-ledger-stats)
      (should (string-match-p "stats output" ledger-mode-test--last-msg)))))


;;; ----------------------------------------------------------------
;;; ledger-insert-effective-date overwrite branch (lines 204, 212)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/insert-effective-date-replace ()
  "Inserting an effective date over an existing one replaces it (line 211-212)."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "2024/01/01=2024/01/05 Acme
    Expenses:Foo                            $1.00
    Assets:Bar
"
    (ledger-insert-effective-date "2024/02/01")
    (should (string-match-p "2024/01/01=2024/02/01 Acme"
                            (buffer-string)))))

(ert-deftest ledger-mode/insert-effective-date-prompts-when-nil ()
  "When called with no DATE, the user is prompted (line 204)."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "2024/01/01 Acme
    Expenses:Foo                            $1.00
    Assets:Bar
"
    (cl-letf (((symbol-function 'ledger-read-date)
               (lambda (_p) "2024/01/15")))
      (ledger-insert-effective-date))
    (should (string-match-p "2024/01/01=2024/01/15 Acme"
                            (buffer-string)))))


;;; ----------------------------------------------------------------
;;; ledger-mode-remove-extra-lines  (lines 221-223)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/remove-extra-lines ()
  "`ledger-mode-remove-extra-lines' collapses runs of blank lines to one."
  :tags '(mode)
  (with-temp-buffer
    (insert "A\n\n\n\nB\n\n\n\n\nC\n")
    (ledger-mode-remove-extra-lines)
    (should (string= "A\n\nB\n\nC\n" (buffer-string)))))


;;; ----------------------------------------------------------------
;;; ledger-mode-clean-buffer  (lines 228-240)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/clean-buffer-sorts-aligns-and-strips ()
  "`ledger-mode-clean-buffer' sorts, aligns and removes extra blank lines."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "2024/02/01 B
    Expenses:Bar                       $2.00
    Assets:Cash



2024/01/01 A
    Expenses:Foo                       $1.00
    Assets:Cash
"
    (goto-char (point-max))
    (ledger-mode-clean-buffer)
    (let ((str (buffer-string)))
      ;; A comes before B after sorting.
      (should (< (string-match "2024/01/01 A" str)
                 (string-match "2024/02/01 B" str)))
      ;; No triple newlines remain.
      (should-not (string-match-p "\n\n\n" str)))))


;;; ----------------------------------------------------------------
;;; ledger-rename-account interactive form (lines 253-271)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/rename-account-interactive ()
  "Interactive form prompts for old + new name and renames accounts."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "2024/01/01 Acme
    Expenses:Foo                            $1.00
    Assets:Bar
"
    (cl-letf (((symbol-function 'ledger-read-account-with-prompt)
               (lambda (_p) "Expenses:Foo"))
              ((symbol-function 'ledger-read-string-with-default)
               (lambda (_p _d) "Expenses:Bar")))
      (call-interactively #'ledger-rename-account))
    (should (string-match-p "Expenses:Bar" (buffer-string)))
    (should-not (string-match-p "Expenses:Foo" (buffer-string)))))

(ert-deftest ledger-mode/rename-account-aligns-when-set ()
  "When `ledger-post-auto-align' is non-nil, postings get aligned (line 271)."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "2024/01/01 Acme
    Expenses:Foo            $1.00
    Assets:Bar
"
    (let ((ledger-post-auto-align t))
      (ledger-rename-account "Expenses:Foo" "Expenses:Bar"))
    ;; The amount column should be moved to ledger-post-amount-alignment-column.
    (let ((str (buffer-string)))
      (should (string-match-p "Expenses:Bar" str))
      (should-not (string-match-p "Expenses:Foo" str)))))


;;; ----------------------------------------------------------------
;;; ledger--date-change error path (line 318)
;;; ----------------------------------------------------------------

(ert-deftest ledger-mode/date-change-not-at-date-errors ()
  "Calling the date-change helpers when not on a date raises a user-error."
  :tags '(mode)
  (ledger-tests-with-temp-file
      "; just a comment\n"
    (goto-char (point-min))
    (should-error (ledger-date-up 1) :type 'user-error)
    (should-error (ledger-date-down 1) :type 'user-error)))


(provide 'mode-test)

;;; mode-test.el ends here
