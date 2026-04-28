;;; commodities-test.el --- ERT for ledger-commodities  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2025 John Wiegley <johnw AT gnu DOT org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;;; Commentary:
;;  Regression tests for ledger-commodities.

;;; Code:
(require 'test-helper)
(require 'ledger-commodities)


;;; ledger-split-commodity-string --------------------------------------

(ert-deftest ledger-commodities/split-period-leading ()
  "Split simple period-decimal amount with leading currency symbol."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (let ((result (ledger-split-commodity-string "$ 12.50")))
      (should (= (car result) 12.5))
      (should (string= (cadr result) "$")))))

(ert-deftest ledger-commodities/split-period-trailing ()
  "Split amount with trailing commodity symbol after the number."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (let ((result (ledger-split-commodity-string "12.50 USD")))
      (should (= (car result) 12.5))
      (should (string= (cadr result) "USD")))))

(ert-deftest ledger-commodities/split-comma-decimal ()
  "Split comma-decimal amount when env alist requests it."
  :tags '(commodities)
  ;; Triggers the decimal-comma branch (line 55).
  (let ((ledger-environment-alist '(("decimal-comma"))))
    (let ((result (ledger-split-commodity-string "10,50 €")))
      (should (= (car result) 10.5))
      (should (string= (cadr result) "€")))))

(ert-deftest ledger-commodities/split-quoted-commodity ()
  "Split a quoted commodity (lines 63-71)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (let ((result (ledger-split-commodity-string "\"FOO\" 100.00")))
      (should (= (car result) 100.0))
      (should (string= (cadr result) "FOO")))))

(ert-deftest ledger-commodities/split-empty-string ()
  "Empty string returns nil (early bail at length check)."
  :tags '(commodities)
  (should (null (ledger-split-commodity-string ""))))

(ert-deftest ledger-commodities/split-zero-balance-fallback ()
  "Match the bare \"0\" fallback (lines 82, 85).
The decimal regex would normally match a stray digit in the middle of
the string, so deliberately use one whose only digit is a zero placed
where the decimal regex cannot consume it as a number."
  :tags '(commodities)
  (let ((ledger-environment-alist nil)
        (ledger-reconcile-default-commodity "$"))
    ;; A '+' prefix prevents the period-decimal regex from matching, but
    ;; the buffer still contains a '0' that the next clause finds.
    (let ((result (ledger-split-commodity-string "+0")))
      ;; Either the zero-fallback path or the regex-match path is fine,
      ;; both return a commoditized 0.
      (should (numberp (car result)))
      (should (= 0 (car result))))))

(ert-deftest ledger-commodities/split-no-match-fallback ()
  "Strings without numbers or zero return default commodity (line 87)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil)
        (ledger-reconcile-default-commodity "$"))
    (let ((result (ledger-split-commodity-string "garbage")))
      (should (= (car result) 0))
      (should (string= (cadr result) "$")))))


;;; ledger-string-balance-to-commoditized-amount ----------------------

(ert-deftest ledger-commodities/string-balance-to-commoditized ()
  "Multi-line balance string parses each line (lines 92-94)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (let ((result (ledger-string-balance-to-commoditized-amount
                   "$ 10.00\n5 USD")))
      (should (= (length result) 2))
      (should (= (car (nth 0 result)) 10.0))
      (should (string= (cadr (nth 0 result)) "$"))
      (should (= (car (nth 1 result)) 5.0))
      (should (string= (cadr (nth 1 result)) "USD")))))


;;; ledger-subtract-commodity / ledger-add-commodity / negate ---------

(ert-deftest ledger-commodities/subtract-zero ()
  "Subtracting a zero commodity returns the first directly."
  :tags '(commodities)
  (should (equal '(10 "$")
                 (ledger-subtract-commodity '(10 "$") '(0 "EUR")))))

(ert-deftest ledger-commodities/subtract-same ()
  "Subtraction with matching commodities."
  :tags '(commodities)
  (should (equal '(7 "$")
                 (ledger-subtract-commodity '(10 "$") '(3 "$")))))

(ert-deftest ledger-commodities/subtract-different-commodities-errors ()
  "Subtracting different non-zero commodities is an error (line 105)."
  :tags '(commodities)
  (should-error
   (ledger-subtract-commodity '(10 "$") '(3 "EUR"))))

(ert-deftest ledger-commodities/add-zero ()
  "Adding zero to a commodity returns it unchanged (line 113)."
  :tags '(commodities)
  (should (equal '(10 "$")
                 (ledger-add-commodity '(10 "$") '(0 "EUR")))))

(ert-deftest ledger-commodities/add-same ()
  "Adding two commodities of the same kind."
  :tags '(commodities)
  (should (equal '(15 "$")
                 (ledger-add-commodity '(10 "$") '(5 "$")))))

(ert-deftest ledger-commodities/add-different-errors ()
  "Adding different commodities errors out."
  :tags '(commodities)
  (should-error
   (ledger-add-commodity '(10 "$") '(5 "EUR"))))

(ert-deftest ledger-commodities/negate ()
  "Negation flips the value but preserves the commodity."
  :tags '(commodities)
  (should (equal '(-3 "$") (ledger-negate-commodity '(3 "$")))))


;;; ledger-string-to-number / ledger-number-to-string -----------------

(ert-deftest ledger-commodities/string-to-number-period ()
  "Period-decimal string parses correctly without env."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (should (= 1234.56 (ledger-string-to-number "1,234.56")))))

(ert-deftest ledger-commodities/string-to-number-decimal-comma ()
  "DECIMAL-COMMA argument forces comma-as-decimal handling (line 134, 137)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    ;; Argument forces decimal-comma.
    (should (= 1234.56 (ledger-string-to-number "1.234,56" t)))))

(ert-deftest ledger-commodities/string-to-number-env-decimal-comma ()
  "Env-driven decimal-comma triggers the same code path."
  :tags '(commodities)
  (let ((ledger-environment-alist '(("decimal-comma"))))
    (should (= 1234.56 (ledger-string-to-number "1.234,56")))))

(ert-deftest ledger-commodities/number-to-string-period ()
  "Default period-decimal output."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (should (string= "12.5" (ledger-number-to-string 12.5)))))

(ert-deftest ledger-commodities/number-to-string-comma ()
  "DECIMAL-COMMA argument replaces \".\" with \",\" (lines 146-147)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (should (string= "12,5" (ledger-number-to-string 12.5 t)))))

(ert-deftest ledger-commodities/number-to-string-env-comma ()
  "Env-driven decimal-comma replaces dots with commas."
  :tags '(commodities)
  (let ((ledger-environment-alist '(("decimal-comma"))))
    (should (string= "12,5" (ledger-number-to-string 12.5)))))


;;; ledger-commodity-to-string ----------------------------------------

(ert-deftest ledger-commodities/commodity-to-string-short ()
  "Single-character commodity is placed before the value."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (should (string= "$ 12.5" (ledger-commodity-to-string '(12.5 "$"))))))

(ert-deftest ledger-commodities/commodity-to-string-long ()
  "Multi-character commodity is placed after the value (line 157)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil))
    (should (string= "12.5 USD"
                     (ledger-commodity-to-string '(12.5 "USD"))))))


;;; ledger-read-commodity-with-prompt (lines 45-49) -------------------

(ert-deftest ledger-commodities/read-commodity-with-prompt ()
  "Read commodity invokes ledger and the completing-read default."
  :tags '(commodities)
  (cl-letf (((symbol-function 'ledger-exec-ledger)
             (lambda (_in out &rest _args)
               (with-current-buffer out
                 (insert "$\nEUR\nUSD\n"))))
            ((symbol-function 'completing-read)
             (lambda (_p _coll &rest _ignore) "USD")))
    (with-temp-buffer
      (let ((ledger-reconcile-default-commodity "$"))
        (should (string= "USD"
                         (ledger-read-commodity-with-prompt "Pick: ")))))))


;;; ledger-read-commodity-string (lines 171-178) ----------------------

(ert-deftest ledger-commodities/read-commodity-string-with-commodity ()
  "Read commodity string returns parsed value with commodity from input."
  :tags '(commodities)
  (let ((ledger-environment-alist nil)
        (ledger-reconcile-default-commodity "$"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "12.50 EUR")))
      (let ((result (ledger-read-commodity-string "Amount")))
        (should (= (car result) 12.5))
        (should (string= (cadr result) "EUR"))))))

(ert-deftest ledger-commodities/read-commodity-string-default-commodity ()
  "When the read string has no commodity, fall back to the default."
  :tags '(commodities)
  (let ((ledger-environment-alist nil)
        (ledger-reconcile-default-commodity "$"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "42.00")))
      (let ((result (ledger-read-commodity-string "Amount")))
        (should (= (car result) 42.0))
        (should (string= (cadr result) "$"))))))

(ert-deftest ledger-commodities/read-commodity-string-empty ()
  "Empty input returns nil (the `when-let' guard fails)."
  :tags '(commodities)
  (let ((ledger-environment-alist nil)
        (ledger-reconcile-default-commodity "$"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) "")))
      (should (null (ledger-read-commodity-string "Amount"))))))


(provide 'commodities-test)

;;; commodities-test.el ends here
