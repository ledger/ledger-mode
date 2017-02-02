;;; post-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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


(ert-deftest ledger-post/test-004 ()
  "Regress test for Bug 932
http://bugs.ledger-cli.org/show_bug.cgi?id=932"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"2000/10/24 * Action France Telecom
    Actif:Courant:BnpCc       50 France_Telecom @@ 10000,00 F
    Actif:Courant:BnpCc
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2000/10/24 * Action France Telecom
    Actif:Courant:BnpCc                           50 France_Telecom @@ 10000,00 F
    Actif:Courant:BnpCc
" ))))


(ert-deftest ledger-post/test-005 ()
  "Regress test for Bug 932
http://bugs.ledger-cli.org/show_bug.cgi?id=932"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"1994/01/01 * Solde initial
    Actif:Courant:CeLivretJeune    10000,00 F = 10000,00 F
    Equity:SoldeInitial
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "1994/01/01 * Solde initial
    Actif:Courant:CeLivretJeune             10000,00 F = 10000,00 F
    Equity:SoldeInitial
" ))))


(ert-deftest ledger-post/test-006 ()
  "Regress test for Bug 932
http://bugs.ledger-cli.org/show_bug.cgi?id=932"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"2000/07/14 * Salomon Smith Barney
    Actif:Invest:SalomonSmithBarney             1,20 $
    Revenu:Invest:Intérêt               -1,20 $ {=1,0702 €}
    Revenu:Devise                              -1,20 $
    Revenu:Devise                       1,20 $ {=1,0702 €}
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2000/07/14 * Salomon Smith Barney
    Actif:Invest:SalomonSmithBarney             1,20 $
    Revenu:Invest:Intérêt                      -1,20 $ {=1,0702 €}
    Revenu:Devise                              -1,20 $
    Revenu:Devise                               1,20 $ {=1,0702 €}
" ))))


(ert-deftest ledger-post/test-007 ()
  "Regress test for Bug 932
http://bugs.ledger-cli.org/show_bug.cgi?id=932"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"2006/06/30 * Employer
    Actif:Invest:Peg         54,7328 Pacteo_Monétaire @@ 1817,13 €
    Revenu:Salaire:Intéressement
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2006/06/30 * Employer
    Actif:Invest:Peg                         54,7328 Pacteo_Monétaire @@ 1817,13 €
    Revenu:Salaire:Intéressement
" ))))


(ert-deftest ledger-post/test-008 ()
  "Regress test for Bug 932
http://bugs.ledger-cli.org/show_bug.cgi?id=932"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"1999/05/22 * PEE
    Actif:Invest:Pee        198,064 \"Arcancia_Securite_254\" @@ 670,05 €
    Actif:Invest:Pee
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
            "1999/05/22 * PEE
    Actif:Invest:Pee                         198,064 \"Arcancia_Securite_254\" @@ 670,05 €
    Actif:Invest:Pee
" ))))


(ert-deftest ledger-post/test-009 ()
  "Regress test for Bug 927
http://bugs.ledger-cli.org/show_bug.cgi?id=927"
  :tags '(post regress)

  (ledger-tests-with-temp-file
      "~ Quarterly
    Dépense:Maison:Maintenance                232,48 €
    Actif:Courant:BnpCc

"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
            "~ Quarterly
    Dépense:Maison:Maintenance                232,48 €
    Actif:Courant:BnpCc

" ))))


(ert-deftest ledger-post/test-010 ()
  "Regress test for Bug 925
http://bugs.ledger-cli.org/show_bug.cgi?id=925"
  :tags '(post regress)

  (ledger-tests-with-temp-file
      "
; isolated comment (nothing before and after)

"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
            "
; isolated comment (nothing before and after)

" ))))


(ert-deftest ledger-post/test-011 ()
  "Regress test for Bug 923+924
http://bugs.ledger-cli.org/show_bug.cgi?id=923
http://bugs.ledger-cli.org/show_bug.cgi?id=924"
  :tags '(post regress)

  (ledger-tests-with-temp-file
      "1994/01/10 * Mother
    Actif:Courant:BnpCc                       500,00 F  ; Étrennes
    Revenu:Autre:CadeauReçu

"
    (indent-region (point-min) (point-max))
    (should
     (equal (buffer-string)
            "1994/01/10 * Mother
    Actif:Courant:BnpCc                       500,00 F  ; Étrennes
    Revenu:Autre:CadeauReçu

" ))))


(ert-deftest ledger-post/test-012 ()
  "Regress test for Bug 1007
http://bugs.ledger-cli.org/show_bug.cgi?id=1007"
  :tags '(post regress)

  (ledger-tests-with-temp-file
   "2014-06-10 * (POS) Walgreen's
	   ; Withdrawal Visa Checking
	   ; TransID: 991332
	   Assets:GTCU:Checking			$-1.000,00
	   Expenses:Entertainment
"
   (goto-char 76)
   (ledger-post-align-xact (point))
   (should
    (equal (buffer-string)
           "2014-06-10 * (POS) Walgreen's
    ; Withdrawal Visa Checking
    ; TransID: 991332
    Assets:GTCU:Checking                  $-1.000,00
    Expenses:Entertainment
" ))))


(ert-deftest ledger-post/test-013 ()
  "Baseline test for ledger-post-amount-alignment-at
Introduced by commit e9f16a1"
  :tags '(post baseline)

  (ledger-tests-with-temp-file
"2010/12/20 * Organic Co-op
  Expenses:Food:Groceries	   1,1 €
  Expenses:Food:Groceries	   1,02 €
  Expenses:Food:Groceries	   1,003 €
  Expenses:Food:Groceries	   1,0004 €
  Assets:Checking		   -4,1234 €
"
    ; ledger-post-amount-alignment-at is kept as default
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2010/12/20 * Organic Co-op
    Expenses:Food:Groceries                      1,1 €
    Expenses:Food:Groceries                     1,02 €
    Expenses:Food:Groceries                    1,003 €
    Expenses:Food:Groceries                   1,0004 €
    Assets:Checking                          -4,1234 €
" )))

  (ledger-tests-with-temp-file
"2010/12/20 * Organic Co-op
  Expenses:Food:Groceries	   1,1 €
  Expenses:Food:Groceries	   1,02 €
  Expenses:Food:Groceries	   1,003 €
  Expenses:Food:Groceries	   1,0004 €
  Assets:Checking		   -4,1234 €
"
    (setq ledger-post-amount-alignment-at :decimal)
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2010/12/20 * Organic Co-op
    Expenses:Food:Groceries                        1,1 €
    Expenses:Food:Groceries                        1,02 €
    Expenses:Food:Groceries                        1,003 €
    Expenses:Food:Groceries                        1,0004 €
    Assets:Checking                               -4,1234 €
" )))

  (ledger-tests-with-temp-file
"2010/12/20 * Organic Co-op
  Expenses:Food:Groceries	   1,1 €
  Expenses:Food:Groceries	   1,02 €
  Expenses:Food:Groceries	   1,003 €
  Expenses:Food:Groceries	   1,0004 €
  Assets:Checking		   -4,1234 €
"
    (setq ledger-post-amount-alignment-at :end)
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2010/12/20 * Organic Co-op
    Expenses:Food:Groceries                      1,1 €
    Expenses:Food:Groceries                     1,02 €
    Expenses:Food:Groceries                    1,003 €
    Expenses:Food:Groceries                   1,0004 €
    Assets:Checking                          -4,1234 €
" )))

 (ledger-tests-with-temp-file
"2015/09/28 Foo
    Expenses:IT                                  100.00 Kc
    Expenses:Travel                                 $10
    Expenses:Travel                                  27 Kc
    Expenses:Books                               213.54 Kc
    Expenses:Groceries                            1.123 Kc
    Assets:Checking:CS                           $10.10
"
    (setq ledger-post-amount-alignment-at :decimal)
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2015/09/28 Foo
    Expenses:IT                                  100.00 Kc
    Expenses:Travel                              $10
    Expenses:Travel                               27 Kc
    Expenses:Books                               213.54 Kc
    Expenses:Groceries                             1.123 Kc
    Assets:Checking:CS                           $10.10
" )))
  )


(ert-deftest ledger-post/test-014 ()
  "Regress test for Bug 946
http://bugs.ledger-cli.org/show_bug.cgi?id=946"
  :tags '(post regress)

  (ledger-tests-with-temp-file
"2013-05-01 foo
    Expenses:Foo                      $10
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                      $10.00
    Assets:Bar

2013-05-01 foo
    Expenses:Foo                      10 €
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                      10.00 €
    Assets:Bar

2013-05-01 foo
    Expenses:Foo                      $-10
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                      $-10.00
    Assets:Bar

2013-05-01 foo
    Expenses:Foo                      -10 €
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                      -10.00 €
    Assets:Bar
"
    (ledger-post-align-postings (point-min) (point-max))
    (should
     (equal (buffer-string)
      "2013-05-01 foo
    Expenses:Foo                                 $10
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013-05-01 foo
    Expenses:Foo                                  10 €
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                               10.00 €
    Assets:Bar

2013-05-01 foo
    Expenses:Foo                                $-10
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                             $-10.00
    Assets:Bar

2013-05-01 foo
    Expenses:Foo                                 -10 €
    Assets:Bar

2013-05-03 foo
    Expenses:Foo                              -10.00 €
    Assets:Bar
" ))))


(provide 'post-test)

;;; post-test.el ends here
