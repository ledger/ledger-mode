;;; context-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-context

;;; Code:
(require 'test-helper)


(ert-deftest ledger-context/test-001 ()
  "only two blank separators between account and amount"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "    Actif:Courant:BnpCc  25,17 €  ; Rembt tissus"
   (goto-char 26)                       ; on the '2' of 25,17
   (let ((context (ledger-context-at-point)))
      (should (eq (ledger-context-current-field context) 'amount))
      (should (equal "25,17"
                     (ledger-context-field-value context 'amount))))))


(ert-deftest ledger-context/test-002 ()
  "amount without decimal separator"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "    Actif:Courant:BnpCc  25 €  ; Rembt tissus"
   (goto-char 19)                       ; on the 'B' of BnpCc
   (let ((context (ledger-context-at-point)))
      (should (eq (ledger-context-current-field context) 'account))
      (should (equal "Actif:Courant:BnpCc"
                     (ledger-context-field-value context 'account))))

   (goto-char 26)                       ; on the '2' of 25
   (let ((context (ledger-context-at-point)))
      (should (eq (ledger-context-current-field context) 'amount))
      (should (equal "25"
                     (ledger-context-field-value context 'amount))))))


(provide 'context-test)

;;; context-test.el ends here
