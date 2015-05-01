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


(provide 'complete-test)

;;; complete-test.el ends here
