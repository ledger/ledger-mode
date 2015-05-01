;;; post-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-post

;;; Code:
(require 'test-helper)


(ert-deftest ledger-post/test-001 ()
  "Regress test for Bug 962
http://bugs.ledger-cli.org/show_bug.cgi?id=962"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"2004/05/14 * Inter Expansion
    Actif:Invest:InterExpansion            -50,873 Taux @@ 3260,55 €
   Actif:Invest:InterExpansion           3260,55 €
     [Revenu:Invest:Capital]          -50,873 Taux @@ 3270,01 €
  [Revenu:Invest:Capital]          50,873 Taux @@ 3260,55 €
      [Revenu:Invest:Capital]
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2004/05/14 * Inter Expansion
    Actif:Invest:InterExpansion              -50,873 Taux @@ 3260,55 €
    Actif:Invest:InterExpansion              3260,55 €
    [Revenu:Invest:Capital]                  -50,873 Taux @@ 3270,01 €
    [Revenu:Invest:Capital]                   50,873 Taux @@ 3260,55 €
    [Revenu:Invest:Capital]
" ))))


(ert-deftest ledger-post/test-002 ()
  "Regress test for Bug 941
http://bugs.ledger-cli.org/show_bug.cgi?id=941"
  :tags '(post regress)

  (ledger-tests-with-temp-file
      "1997/06/08 * CCIT
    Dépense:Autre:Professionnel:NonRemboursé  72,00 F
    Actif:Courant:Banque
"
    (setq ledger-post-amount-alignment-column 70)
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
            "1997/06/08 * CCIT
    Dépense:Autre:Professionnel:NonRemboursé                     72,00 F
    Actif:Courant:Banque
" ))
    (setq ledger-post-amount-alignment-column 45)
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "1997/06/08 * CCIT
    Dépense:Autre:Professionnel:NonRemboursé  72,00 F
    Actif:Courant:Banque
" ))))


(ert-deftest ledger-post/test-003 ()
  "Regress test for Bug 933
http://bugs.ledger-cli.org/show_bug.cgi?id=933"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"2013/02/06 Travelex
    Actif:Remboursement:Employer       ;    50,00 £
    * Passif:Crédit:Employer:BnpVisa       -61,96 €
;    Revenu:Devise                         -50,00 £
;    Revenu:Devise                          61,96 €
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2013/02/06 Travelex
    Actif:Remboursement:Employer       ;       50,00 £
    * Passif:Crédit:Employer:BnpVisa          -61,96 €
;    Revenu:Devise                         -50,00 £
;    Revenu:Devise                          61,96 €
" ))))                                  ; FIXME amount after a ';' should not be aligned. Introduced with f89c348


(provide 'post-test)

;;; post-test.el ends here
