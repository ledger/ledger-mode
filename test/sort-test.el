;;; sort-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-sort

;;; Code:
(require 'test-helper)


(ert-deftest ledger-sort/test-001 ()
  "Regress test for Bug 937
http://bugs.ledger-cli.org/show_bug.cgi?id=937"
  :tags '(sort regress)

  (ledger-tests-with-temp-file
      "1994/01/06=1994/01/01 * Retrait
    Dépense:Liquide                         300,00 F
    Actif:Courant:BnpCc

1994/01/06 * Caisse d'Allocations Familliales
    Actif:Courant:BnpCc                     896,00 F = 9645,42 F
    Revenu:Autre:AllocationFamiliale
"
    (ledger-sort-buffer)
    (should
     (equal (buffer-string)
            "1994/01/06=1994/01/01 * Retrait
    Dépense:Liquide                         300,00 F
    Actif:Courant:BnpCc

1994/01/06 * Caisse d'Allocations Familliales
    Actif:Courant:BnpCc                     896,00 F = 9645,42 F
    Revenu:Autre:AllocationFamiliale
" ))))


(ert-deftest ledger-sort/test-002 ()
  "Regress test for Bug 937
http://bugs.ledger-cli.org/show_bug.cgi?id=937"
  :tags '(sort regress)

  (ledger-tests-with-temp-file
      "2013/02/27 Paul
    Actif:Remboursement:Employer              5,30 €
    * Passif:Crédit:Employer:BnpVisa

2013/02/27 Checker Cars
    Actif:Remboursement:Airbus               42,33 €
    * Passif:Crédit:Employer:BnpVisa
"
    (ledger-sort-buffer)
    (should
     (equal (buffer-string)
            "2013/02/27 Paul
    Actif:Remboursement:Employer              5,30 €
    * Passif:Crédit:Employer:BnpVisa

2013/02/27 Checker Cars
    Actif:Remboursement:Airbus               42,33 €
    * Passif:Crédit:Employer:BnpVisa
" ))))


(ert-deftest ledger-sort/test-003 ()
  "Regress test for Bug 260
http://bugs.ledger-cli.org/show_bug.cgi?id=260"
  :tags '(sort regress)

  (ledger-tests-with-temp-file
   "2013/05/03 foo
    Expenses:Foo                             3,00 €
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                               2,00 €
    Assets:Bar

2013/05/01 foo
    Expenses:Foo                             1,00 €
    Assets:Bar
"
   (ledger-sort-region (point-min) 168) ; first two transactions
   (should (equal (buffer-string)
                  "2013/05/02 foo
    Expenses:Foo                               2,00 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             3,00 €
    Assets:Bar

2013/05/01 foo
    Expenses:Foo                             1,00 €
    Assets:Bar
"))
   (ledger-sort-region 85 (point-max))  ; last two transactions
   (should (equal (buffer-string)
                  "2013/05/02 foo
    Expenses:Foo                               2,00 €
    Assets:Bar

2013/05/01 foo
    Expenses:Foo                             1,00 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             3,00 €
    Assets:Bar
"))))


(provide 'sort-test)

;;; sort-test.el ends here
