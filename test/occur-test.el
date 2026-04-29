;;; occur-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-occur

;;; Code:
(require 'test-helper)


(ert-deftest ledger-occur/test-001 ()
  "Regress test for Bug 246
http://bugs.ledger-cli.org/show_bug.cgi?id=246"
  :tags '(occur regress)

  (ledger-tests-with-temp-file
      "2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking

2011/01/05 Employer
  * Assets:Checking                 $ 2000.00
  Income:Salary
"
    (ledger-occur "Groceries")
    (should
     (equal (ledger-test-visible-buffer-string)
            "2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking
"))))

(ert-deftest ledger-occur/test-002 ()
  "Regression test for #54.
https://github.com/ledger/ledger-mode/issues/54"
  :tags '(occur regress)

  (ledger-tests-with-temp-file
      "\
2024-03-12 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking

2024-03-15 Employer
  * Assets:Checking           $2000.00
  Income:Salary

2024-03-19 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking
"
    (ledger-occur "Groceries")
    (should
     (equal (ledger-test-visible-buffer-string)
            "\
2024-03-12 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking

2024-03-19 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking
"))

    (setq ledger-occur-use-face-shown t)
    (goto-char (point-min))
    (search-forward "2024-03-12")
    (should (eq (get-char-property (point) 'font-lock-face)
                'ledger-occur-xact-face))
    (search-forward "2024-03-19")
    (should (eq (get-char-property (point) 'font-lock-face)
                'ledger-occur-xact-face))))


(ert-deftest ledger-occur/test-003 ()
  "Additional tests for various edge cases."
  :tags '(occur regress)

  (ledger-tests-with-temp-file
      "\
2024-03-12 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking

2024-03-15 Employer
  * Assets:Checking           $2000.00
  Income:Salary

2024-03-19 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking
"
    ;; invisible on both sides of a visible portion
    (ledger-occur "Employer")
    (should
     (equal (ledger-test-visible-buffer-string)
            "\

2024-03-15 Employer
  * Assets:Checking           $2000.00
  Income:Salary
"))

    ;; no matches
    (ledger-occur "zzzzzz")
    (should
     (equal (ledger-test-visible-buffer-string)
            "\
2024-03-12 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking

2024-03-15 Employer
  * Assets:Checking           $2000.00
  Income:Salary

2024-03-19 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking
"))))


(ert-deftest ledger-occur/test-004 ()
  "Regression test for #415
https://github.com/ledger/ledger-mode/issues/415"
  :tags '(occur regress)

  (ledger-tests-with-temp-file
      "\
2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking

2011/01/05 Employer
  * Assets:Checking                 $ 2000.00
  Income:Salary
"

    (save-buffer)
    (ledger-occur "Groceries")
    (should
     (equal
      (ledger-test-visible-buffer-string)
      "\
2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking
"))
    (revert-buffer t t)
    (should
     (equal
      (ledger-test-visible-buffer-string)
      "\
2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking

2011/01/05 Employer
  * Assets:Checking                 $ 2000.00
  Income:Salary
"))))


(ert-deftest ledger-occur/test-empty-regex-clears ()
  "Calling `ledger-occur' with empty or nil regex turns off occur mode."
  :tags '(occur)
  (ledger-tests-with-temp-file
      "\
2024-03-12 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking

2024-03-15 Employer
  * Assets:Checking           $2000.00
  Income:Salary
"
    (ledger-occur "Groceries")
    (should ledger-occur-mode)
    (ledger-occur "")
    (should-not ledger-occur-mode)

    (ledger-occur "Groceries")
    (should ledger-occur-mode)
    (ledger-occur nil)
    (should-not ledger-occur-mode)))


(ert-deftest ledger-occur/test-interactive ()
  "Calling `ledger-occur' interactively prompts for the regexp."
  :tags '(occur)
  (ledger-tests-with-temp-file
      "\
2024-03-12 Grocery Store
  Expenses:Food:Groceries     $50
  Assets:Checking
"
    (cl-letf (((symbol-function 'read-regexp)
               (lambda (&rest _) "Groceries")))
      (call-interactively #'ledger-occur)
      (should ledger-occur-mode)
      (should (equal "Groceries" ledger-occur-current-regex)))))


(ert-deftest ledger-occur/test-prompt-region ()
  "`ledger-occur-prompt' returns active region when single-line."
  :tags '(occur)
  (ledger-tests-with-temp-file
      "Hello World\n"
    (goto-char (point-min))
    (push-mark (point) t t)
    (forward-char 5)                     ; select "Hello"
    (activate-mark)
    (should (equal "Hello" (ledger-occur-prompt)))
    (deactivate-mark)))


(ert-deftest ledger-occur/test-prompt-multiline-region ()
  "`ledger-occur-prompt' returns nil when region spans multiple lines."
  :tags '(occur)
  (ledger-tests-with-temp-file
      "first\nsecond\n"
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char (point-max))
    (activate-mark)
    (should (null (ledger-occur-prompt)))
    (deactivate-mark)))


(ert-deftest ledger-occur/test-prompt-current-word ()
  "`ledger-occur-prompt' returns current word when no region active."
  :tags '(occur)
  (ledger-tests-with-temp-file
      "Hello\n"
    (goto-char (point-min))
    (forward-char 1)                     ; inside "Hello"
    (should (equal "Hello" (ledger-occur-prompt)))))


(ert-deftest ledger-occur/test-compress-adjacent ()
  "`ledger-occur-compress-matches' merges adjacent xacts into one entry."
  :tags '(occur)
  ;; Two xact bounds with end+1 == next-begin should be compressed.
  (let ((result (ledger-occur-compress-matches '((1 10) (11 20) (100 110)))))
    ;; (11 - 10) < 2 → first two get merged into (1 20). Then (100 110) is a
    ;; separate entry.
    (should (equal result '((1 20) (100 110))))))


(provide 'occur-test)

;;; occur-test.el ends here
