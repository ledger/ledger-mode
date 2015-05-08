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


(ert-deftest ledger-context/test-003 ()
  "number in comments"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "    Dépense:Impôt:Local                      48,00 €  ; TH 2013"
   (should (equal (ledger-context-at-point)
                  '(acct-transaction indent ((indent "    " 1) (status nil nil) (account "Dépense:Impôt:Local" 5) (separator "                      " 24) (amount "48,00" 46) (commodity "€" 52) (comment "TH 2013" 57)))))))


(ert-deftest ledger-context/test-004 ()
  "Regress test for Bug 947
http://bugs.ledger-cli.org/show_bug.cgi?id=947"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "2014/11/10  EDF
    * Dépense:Maison:Service:Électricité    36,23 €
    Actif:Courant:BnpCc

2014/11/14  Banque Accord Retrait
    ! Passif:Crédit:BanqueAccord              60,00 €
    Actif:Courant:BnpCc
"
   (goto-char 34)                      ; line 2, on Maison
   (let ((context (ledger-context-at-point)))
     (should (eq (ledger-context-current-field context) 'account))
     (should (equal "Dépense:Maison:Service:Électricité"
                    (ledger-context-field-value context 'account))))
   (goto-char 154)                     ; line 6, on Accord
   (let ((context (ledger-context-at-point)))
     (should (eq (ledger-context-current-field context) 'account))
     (should (equal "Passif:Crédit:BanqueAccord"
                    (ledger-context-field-value context 'account))))))


(provide 'context-test)

;;; context-test.el ends here
