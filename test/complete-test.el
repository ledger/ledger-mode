;;; complete-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-complete

;;; Code:
(require 'test-helper)


(ert-deftest ledger-complete/test-001 ()
  "Regress test for Bug 969+582
http://bugs.ledger-cli.org/show_bug.cgi?id=969
http://bugs.ledger-cli.org/show_bug.cgi?id=582"
  :tags '(complete regress)

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
   (call-interactively #'ledger-magic-tab)
   (should
    (equal (buffer-string)
           "2013/05/19 Retrait
    Dépense:Alimentation:Épicerie  35 €  ; Marché
    Dépense:
    Dépense:Alimentation:Épicerie  8,1 €  ; Arum café
    Dépense:Liquide
    * Passif:Crédit:BanqueAccord              -60,00 €"))))


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
    (equal (buffer-string)
           "2010/04/08 payee
    account1                1 €
    account2

2016/09/01 payee
    account1                1 €
    account2
"))))


(provide 'complete-test)

;;; complete-test.el ends here
