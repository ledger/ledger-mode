;;; sort-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
