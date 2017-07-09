;;; fontify-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-fontify

;;; Code:
(require 'test-helper)


;; --------------------------------------------------------------------
;; Font lock tests
;; --------------------------------------------------------------------
;;
;; For the below tests ledger-fontify/test-XXX, use the following
;; example in Lisp Interaction mode, and at the end of the sexp, type
;; "C-u 0 C-j" to get the result.
;;
;;     (ledger-test-group-str-by-face
;;      "
;;     2010/12/01 * Checking balance
;;       Assets:Checking                   $1,000.00
;;       Equity:Opening Balances")


(ert-deftest ledger-fontify/test-001 ()
  "1st transaction from /path/to/ledger/test/input/demo.ledger"
  :tags '(font regress)

  (ledger-test-font-lock
   "
2010/12/01 * Checking balance
  Assets:Checking                   $1,000.00
  Equity:Opening Balances
"
   '("2010/12/01"               ledger-font-posting-date-face
     "Checking balance"         ledger-font-payee-cleared-face
     "Assets:Checking"          ledger-font-posting-account-face
     "$1,000.00"                ledger-font-posting-amount-face
     "Equity:Opening Balances"  ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-002 ()
  "posting ending with a comment
https://groups.google.com/d/msg/ledger-cli/FcYG5cnFOpw/PmpLq_dzdYwJ"
  :tags '(font regress)

  (ledger-test-font-lock
   "
2010/12/01 * Checking balance
  Assets:Checking                   $42.00 ; the answer to life
  Equity:Opening Balances
"
   '("2010/12/01"               ledger-font-posting-date-face
     "Checking balance"         ledger-font-payee-cleared-face
     "Assets:Checking"          ledger-font-posting-account-face
     "$42.00"                   ledger-font-posting-amount-face
     "; the answer to life"     ledger-font-comment-face
     "Equity:Opening Balances"  ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-003 ()
  "expressions and balance assignments without amount before
https://groups.google.com/d/msg/ledger-cli/FcYG5cnFOpw/856tmpSFbNcJ"
  :tags '(font regress)

  (ledger-test-font-lock
   "
2010/12/01 * Checking balance
  Assets:Checking              (1/4 * $123.00)
  Equity:Opening Balances            = $500.00
"
   '("2010/12/01"               ledger-font-posting-date-face
     "Checking balance"         ledger-font-payee-cleared-face
     "Assets:Checking"          ledger-font-posting-account-face
     "(1/4 * $123.00)"          ledger-font-posting-amount-face
     "Equity:Opening Balances"  ledger-font-posting-account-face
     "= $500.00"                ledger-font-posting-amount-face)))


(ert-deftest ledger-fontify/test-004 ()
  "ledger-mode does not colorize effective date transactions as other entries
https://groups.google.com/d/msg/ledger-cli/cBy3-QSai6o/qoEUGLfcZuUJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2014-11-29 colruyt
    k:eten:olie                     (€11.52 - €2.00)
    t:colruyt                                 €-9.52
2014-12-03=2014-11-29 colruyt
    t:colruyt                                 €35.90
    b:bkbc:cmfrt                             €-35.90
2014-12-03=2014-11-29 colruyt
    t:colruyt                                  €9.52
    b:bkbc:cmfrt                              €-9.52
2014-12-01 azg
    k:varia-k:filantropie:azg                  €7.44
"
 '("2014-11-29"                 ledger-font-posting-date-face
   "colruyt"                    ledger-font-payee-uncleared-face
   "k:eten:olie"                ledger-font-posting-account-face
   "(€11.52 - €2.00)"           ledger-font-posting-amount-face
   "t:colruyt"                  ledger-font-posting-account-face
   "€-9.52"                     ledger-font-posting-amount-face
   "2014-12-03=2014-11-29"      ledger-font-posting-date-face
   "colruyt"                    ledger-font-payee-uncleared-face
   "t:colruyt"                  ledger-font-posting-account-face
   "€35.90"                     ledger-font-posting-amount-face
   "b:bkbc:cmfrt"               ledger-font-posting-account-face
   "€-35.90"                    ledger-font-posting-amount-face
   "2014-12-03=2014-11-29"      ledger-font-posting-date-face
   "colruyt"                    ledger-font-payee-uncleared-face
   "t:colruyt"                  ledger-font-posting-account-face
   "€9.52"                      ledger-font-posting-amount-face
   "b:bkbc:cmfrt"               ledger-font-posting-account-face
   "€-9.52"                     ledger-font-posting-amount-face
   "2014-12-01"                 ledger-font-posting-date-face
   "azg"                        ledger-font-payee-uncleared-face
   "k:varia-k:filantropie:azg"  ledger-font-posting-account-face
   "€7.44"                      ledger-font-posting-amount-face)))


(ert-deftest ledger-fontify/test-005 ()
  "ledger-mode does not colorize effective date transactions as other entries
https://groups.google.com/d/msg/ledger-cli/cBy3-QSai6o/_GeGBvRxLukJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2014-11-26 0bbde77a
    ea91d4b4:e45cece9:55d77630:6fe97d2b        A4.13
    155ee966:be2467c7:e75bfd51:ce9edcc0       A-4.13

2014-11-27=2014-11-15 * e92211c9
    94f2b492:e361e00d:6e702d4d:13531a9d       A38.98
    e6ee9cc7:b91c4f8d:5746af7b:9e12005e
"
 '("2014-11-26"                           ledger-font-posting-date-face
   "0bbde77a"                             ledger-font-payee-uncleared-face
   "ea91d4b4:e45cece9:55d77630:6fe97d2b"  ledger-font-posting-account-face
   "A4.13"                                ledger-font-posting-amount-face
   "155ee966:be2467c7:e75bfd51:ce9edcc0"  ledger-font-posting-account-face
   "A-4.13"                               ledger-font-posting-amount-face
   "2014-11-27=2014-11-15"                ledger-font-posting-date-face
   "e92211c9"                             ledger-font-payee-cleared-face
   "94f2b492:e361e00d:6e702d4d:13531a9d"  ledger-font-posting-account-face
   "A38.98"                               ledger-font-posting-amount-face
   "e6ee9cc7:b91c4f8d:5746af7b:9e12005e"  ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-006 ()
  "emacs fontification issues
https://groups.google.com/d/msg/ledger-cli/UsFDEytFdak/lymuTW5wX9IJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2006/03/10 Opening balance
  Assets:X  £4000
  Equity:Opening Balance

P 2006/03/05 FOO £1.40
P 2006/03/06 BAR £1.50


2006/05/15 Interest
  Income:Taxable:Interest  £-28.75
  Expenses:Tax:Income Tax  £5.75
  Assets:X
"
 '("2006/03/10"               ledger-font-posting-date-face
   "Opening balance"          ledger-font-payee-uncleared-face
   "Assets:X"                 ledger-font-posting-account-face
   "£4000"                    ledger-font-posting-amount-face
   "Equity:Opening Balance"   ledger-font-posting-account-face
   "P"                        ledger-font-price-directive-face
   "2006/03/05"               ledger-font-price-date-face
   "FOO"                      ledger-font-price-symbol-face
   "£1.40"                    ledger-font-price-face
   "P"                        ledger-font-price-directive-face
   "2006/03/06"               ledger-font-price-date-face
   "BAR"                      ledger-font-price-symbol-face
   "£1.50"                    ledger-font-price-face
   "2006/05/15"               ledger-font-posting-date-face
   "Interest"                 ledger-font-payee-uncleared-face
   "Income:Taxable:Interest"  ledger-font-posting-account-face
   "£-28.75"                  ledger-font-posting-amount-face
   "Expenses:Tax:Income Tax"  ledger-font-posting-account-face
   "£5.75"                    ledger-font-posting-amount-face
   "Assets:X"                 ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-007 ()
  "highlighting of files containing only account directives
https://groups.google.com/d/msg/ledger-cli/yNHQHN60qo0/XaDLxAHF92UJ"
  :tags '(font regress)

  (ledger-test-font-lock
   "
account Assets:Bank1:Checking
    note foo
    ; country: it
    ; IBAN: XXX
    ; ledger-file: current.ledger

account Assets:Bank5:LDD
    note foo bar baz
    note foo bar baz
    ; country: fr
    ; cmut-id: XXX
    ; boobank-id: YYY
"
 '("account"                        ledger-font-account-directive-face
   "Assets:Bank1:Checking"          ledger-font-account-name-face
   "note"                           ledger-font-note-directive-face
   "foo"                            ledger-font-note-text-face
   "; country: it"                  ledger-font-comment-face
   "; IBAN: XXX"                    ledger-font-comment-face
   "; ledger-file: current.ledger"  ledger-font-comment-face
   "account"                        ledger-font-account-directive-face
   "Assets:Bank5:LDD"               ledger-font-account-name-face
   "note"                           ledger-font-note-directive-face
   "foo bar baz"                    ledger-font-note-text-face
   "note"                           ledger-font-note-directive-face
   "foo bar baz"                    ledger-font-note-text-face
   "; country: fr"                  ledger-font-comment-face
   "; cmut-id: XXX"                 ledger-font-comment-face
   "; boobank-id: YYY"              ledger-font-comment-face)))


(ert-deftest ledger-fontify/test-008 ()
  "use of alternate dates
https://groups.google.com/d/msg/ledger-cli/yNHQHN60qo0/XaDLxAHF92UJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2014/11/02 * beer
    Expenses:Drinks                                5 EUR
    Assets:Checking                               -5 EUR

2014/11/02=2014/11/03 * salary
    Assets:Checking                               10 EUR
    Income                                       -10 EUR

2014/11/02 * burger
    Expenses:Food                                  7 EUR
    Assets:Checking                               -7 EUR
"
 '("2014/11/02"             ledger-font-posting-date-face
   "beer"                   ledger-font-payee-cleared-face
   "Expenses:Drinks"        ledger-font-posting-account-face
   "5 EUR"                  ledger-font-posting-amount-face
   "Assets:Checking"        ledger-font-posting-account-face
   "-5 EUR"                 ledger-font-posting-amount-face
   "2014/11/02=2014/11/03"  ledger-font-posting-date-face
   "salary"                 ledger-font-payee-cleared-face
   "Assets:Checking"        ledger-font-posting-account-face
   "10 EUR"                 ledger-font-posting-amount-face
   "Income"                 ledger-font-posting-account-face
   "-10 EUR"                ledger-font-posting-amount-face
   "2014/11/02"             ledger-font-posting-date-face
   "burger"                 ledger-font-payee-cleared-face
   "Expenses:Food"          ledger-font-posting-account-face
   "7 EUR"                  ledger-font-posting-amount-face
   "Assets:Checking"        ledger-font-posting-account-face
   "-7 EUR"                 ledger-font-posting-amount-face)))


(ert-deftest ledger-fontify/test-009 ()
  "Several use cases:
- series of periodic transactions
- commodity lines
- transactions with effective dates
- transactions with no amount on the first account line
https://groups.google.com/d/msg/ledger-cli/yNHQHN60qo0/5j7vFX4j74sJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
; Budget ========================================================

~ Weekly
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15

~ Biweekly until 2014-04-03
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15

~ Biweekly from 2014-04-03 until 2014-09-18
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15

; Pricing =======================================================

P 2014-01-01 WRCOM $100.00
P 2014-01-01 VFDAY $100.00
P 2014-01-01 VHDAY $100.00
P 2014-01-01 WBDES $100.00

; 2014-11 =======================================================

2014-11-01 * 921edb9b
    ; 53dd031c
    b9f7b467:53dd031c:379c2da8              $-100.00
    df0fcf15:59c361db:4972bdf0:f3e78683

2014-11-01 * 921edb9b
    ; 53dd031c
    b9f7b467:53dd031c:379c2da8
    df0fcf15:59c361db:4972bdf0:f3e78683      $100.00

2014-11-01=2014-11-02 * 921edb9b
    ; 53dd031c
    b9f7b467:53dd031c:379c2da8              $-100.00
    df0fcf15:59c361db:4972bdf0:f3e78683
"
 '("; Budget ========================================================" ledger-font-comment-face
    "~ Weekly" ledger-font-periodic-xact-face
    "b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
    "$100.00" ledger-font-posting-amount-face
    "df0fcf15" ledger-font-posting-account-face
    "~ Biweekly until 2014-04-03" ledger-font-periodic-xact-face
    "b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
    "$100.00" ledger-font-posting-amount-face
    "df0fcf15" ledger-font-posting-account-face
    "~ Biweekly from 2014-04-03 until 2014-09-18" ledger-font-periodic-xact-face
    "b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
    "$100.00" ledger-font-posting-amount-face
    "df0fcf15" ledger-font-posting-account-face
    "; Pricing =======================================================" ledger-font-comment-face
"P" ledger-font-price-directive-face
"2014-01-01" ledger-font-price-date-face
"WRCOM" ledger-font-price-symbol-face
"$100.00" ledger-font-price-face
"P" ledger-font-price-directive-face
"2014-01-01" ledger-font-price-date-face
"VFDAY" ledger-font-price-symbol-face
"$100.00" ledger-font-price-face
"P" ledger-font-price-directive-face
"2014-01-01" ledger-font-price-date-face
"VHDAY" ledger-font-price-symbol-face
"$100.00" ledger-font-price-face
"P" ledger-font-price-directive-face
"2014-01-01" ledger-font-price-date-face
"WBDES" ledger-font-price-symbol-face
"$100.00" ledger-font-price-face
"; 2014-11 =======================================================" ledger-font-comment-face
"2014-11-01" ledger-font-posting-date-face
"921edb9b" ledger-font-payee-cleared-face
"; 53dd031c" ledger-font-comment-face
"b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
"$-100.00" ledger-font-posting-amount-face
"df0fcf15:59c361db:4972bdf0:f3e78683" ledger-font-posting-account-face
"2014-11-01" ledger-font-posting-date-face
"921edb9b" ledger-font-payee-cleared-face
"; 53dd031c" ledger-font-comment-face
"b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
"df0fcf15:59c361db:4972bdf0:f3e78683" ledger-font-posting-account-face
"$100.00" ledger-font-posting-amount-face
"2014-11-01=2014-11-02" ledger-font-posting-date-face
"921edb9b" ledger-font-payee-cleared-face
"; 53dd031c" ledger-font-comment-face
"b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
"$-100.00" ledger-font-posting-amount-face
"df0fcf15:59c361db:4972bdf0:f3e78683" ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-011 ()
  "zero in some auto-generated postings
https://groups.google.com/d/msg/ledger-cli/9zyWZW_fJmk/nFCm2sthtfAJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2014/03/11 Whatever
    A                                               0
    B                                           6,75€
    C                                          -6,75€
"
 '("2014/03/11"  ledger-font-posting-date-face
   "Whatever"    ledger-font-payee-uncleared-face
   "A"           ledger-font-posting-account-face
   "0"           ledger-font-posting-amount-face
   "B"           ledger-font-posting-account-face
   "6,75€"       ledger-font-posting-amount-face
   "C"           ledger-font-posting-account-face
   "-6,75€"      ledger-font-posting-amount-face)))


(ert-deftest ledger-fontify/test-012 ()
  "no space after cash
https://groups.google.com/d/msg/ledger-cli/9zyWZW_fJmk/92yhmm3lk0UJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2014/08/23 demo fontification error
    Expense:AAA                              € 10,00
    Expense:BBB                              € 20,00
    cash
    Expense:CCC                              € 10,00
    card                                    -€ 10,00
"
 '("2014/08/23"                ledger-font-posting-date-face
   "demo fontification error"  ledger-font-payee-uncleared-face
   "Expense:AAA"               ledger-font-posting-account-face
   "€ 10,00"                   ledger-font-posting-amount-face
   "Expense:BBB"               ledger-font-posting-account-face
   "€ 20,00"                   ledger-font-posting-amount-face
   "cash"                      ledger-font-posting-account-face
   "Expense:CCC"               ledger-font-posting-account-face
   "€ 10,00"                   ledger-font-posting-amount-face
   "card"                      ledger-font-posting-account-face
   "-€ 10,00"                  ledger-font-posting-amount-face)))


(ert-deftest ledger-fontify/test-013 ()
  "alias
https://groups.google.com/d/msg/ledger-cli/Ol0W6BpabPw/UcXU-yAVldkJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
account Assets:StarOne Savings
  alias starone

2014/05/07   CoinStar Jar of Coins
    cash     -$27.69
    starone   $27.69
    cash     -$7.96
    starone   $7.96
    cash     -$156.89
    starone   $156.89
    cash      -$45.13
    starone    $45.13
"
 '("account"                 ledger-font-account-directive-face
   "Assets:StarOne Savings"  ledger-font-account-name-face
   "alias"                   ledger-font-alias-directive-face
   "starone"                 ledger-font-account-name-face
   "2014/05/07"              ledger-font-posting-date-face
   "CoinStar Jar of Coins"   ledger-font-payee-uncleared-face
   "cash"                    ledger-font-posting-account-face
   "-$27.69"                 ledger-font-posting-amount-face
   "starone"                 ledger-font-posting-account-face
   "$27.69"                  ledger-font-posting-amount-face
   "cash"                    ledger-font-posting-account-face
   "-$7.96"                  ledger-font-posting-amount-face
   "starone"                 ledger-font-posting-account-face
   "$7.96"                   ledger-font-posting-amount-face
   "cash"                    ledger-font-posting-account-face
   "-$156.89"                ledger-font-posting-amount-face
   "starone"                 ledger-font-posting-account-face
   "$156.89"                 ledger-font-posting-amount-face
   "cash"                    ledger-font-posting-account-face
   "-$45.13"                 ledger-font-posting-amount-face
   "starone"                 ledger-font-posting-account-face
   "$45.13"                  ledger-font-posting-amount-face)))


(ert-deftest ledger-fontify/test-014 ()
  "first and second transactions are *both* cleared
https://groups.google.com/d/msg/ledger-cli/tLKIzj02XZA/8f1cT5vS6DkJ"
  :tags '(font regress)

  (ledger-test-font-lock
 "
2011/05/08 * Some expense
  Wallets:Some Person  $9.31
  Income:Some income

2011/05/17 * Another expense
  Wallets:Some Person  $10.38
  Income:Some income

2011/09/11 Another expense
  Expenses:Expensive  $585.00
  Wallets:Some Person
"
 '("2011/05/08"           ledger-font-posting-date-face
   "Some expense"         ledger-font-payee-cleared-face
   "Wallets:Some Person"  ledger-font-posting-account-face
   "$9.31"                ledger-font-posting-amount-face
   "Income:Some income"   ledger-font-posting-account-face
   "2011/05/17"           ledger-font-posting-date-face
   "Another expense"      ledger-font-payee-cleared-face
   "Wallets:Some Person"  ledger-font-posting-account-face
   "$10.38"               ledger-font-posting-amount-face
   "Income:Some income"   ledger-font-posting-account-face
   "2011/09/11"           ledger-font-posting-date-face
   "Another expense"      ledger-font-payee-uncleared-face
   "Expenses:Expensive"   ledger-font-posting-account-face
   "$585.00"              ledger-font-posting-amount-face
   "Wallets:Some Person"  ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-015 ()
  "Bug 479 - Descriptions of cleared transactions are not highlighted"
  :tags '(font regress)

  (ledger-test-font-lock
   "
2010/12/01 * Checking balance  ; note
  Assets:Checking                   $1,000.00
  Equity:Opening Balances

2010/12/01 Checking balance  ; note
  Assets:Checking                   $1,000.00
  Equity:Opening Balances
"
   '("2010/12/01"               ledger-font-posting-date-face
     "Checking balance"         ledger-font-payee-cleared-face
     "; note"                   ledger-font-comment-face
     "Assets:Checking"          ledger-font-posting-account-face
     "$1,000.00"                ledger-font-posting-amount-face
     "Equity:Opening Balances"  ledger-font-posting-account-face
     "2010/12/01"               ledger-font-posting-date-face
     "Checking balance"         ledger-font-payee-uncleared-face
     "; note"                   ledger-font-comment-face
     "Assets:Checking"          ledger-font-posting-account-face
     "$1,000.00"                ledger-font-posting-amount-face
     "Equity:Opening Balances"  ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-016 ()
  "Bug 884 - ledger-font-highlight-face does not highlight first line of buffer"
  :tags '(font regress)

  (ledger-test-font-lock
   ;; no space is intentional between '"' and '2' at beginning of below line
   "2013/01/03 * (6106152) Dr About
    Dépense:Santé:Médecin                    23,00 €
    Actif:Courant:BnpCc
"
   '("2013/01/03"             ledger-font-posting-date-face
     "(6106152)"              ledger-font-code-face
     "Dr About"               ledger-font-payee-cleared-face
     "Dépense:Santé:Médecin"  ledger-font-posting-account-face
     "23,00 €"                ledger-font-posting-amount-face
     "Actif:Courant:BnpCc"    ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-017 ()
  "ledger-fontify-xact-state-overrides
https://groups.google.com/d/msg/ledger-cli/9zyWZW_fJmk/G56uVsqv0FAJ"
  :tags '(font regress)

  (let ((str "
~ Weekly
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15

~ Biweekly until 2014-04-03
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15

~ Biweekly from 2014-04-03 until 2014-09-18
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15

= /^(?:Income:|Expenses:(?:Business|Rent$|Furnishings|Taxes|Insurance))/
  (Liabilities:Huququ'llah)               0.19

2010/12/01 * Checking balance  ; note
  Assets:Checking                   $1,000.00
  Equity:Opening Balances

2010/12/01 ! Checking balance  ; note
  Assets:Checking                   $1,000.00
  Equity:Opening Balances

2010/12/01 Checking balance  ; note
  Assets:Checking                   $1,000.00
  Equity:Opening Balances
")
        (face-groups
         '("~ Weekly\n    b9f7b467:53dd031c:379c2da8               $100.00\n    df0fcf15" ledger-font-periodic-xact-face
           "~ Biweekly until 2014-04-03\n    b9f7b467:53dd031c:379c2da8               $100.00\n    df0fcf15" ledger-font-periodic-xact-face
           "~ Biweekly from 2014-04-03 until 2014-09-18\n    b9f7b467:53dd031c:379c2da8               $100.00\n    df0fcf15" ledger-font-periodic-xact-face
           "= /^(?:Income:|Expenses:(?:Business|Rent$|Furnishings|Taxes|Insurance))/\n  (Liabilities:Huququ'llah)               0.19" ledger-font-auto-xact-face
           "2010/12/01 * Checking balance  ; note\n  Assets:Checking                   $1,000.00\n  Equity:Opening Balances" ledger-font-xact-cleared-face
           "2010/12/01 ! Checking balance  ; note\n  Assets:Checking                   $1,000.00\n  Equity:Opening Balances" ledger-font-xact-pending-face
           "2010/12/01"               ledger-font-posting-date-face
           "Checking balance"         ledger-font-payee-uncleared-face
           "; note"                   ledger-font-comment-face
           "Assets:Checking"          ledger-font-posting-account-face
           "$1,000.00"                ledger-font-posting-amount-face
           "Equity:Opening Balances"  ledger-font-posting-account-face)))

    (with-temp-buffer
      (ledger-mode)
      (unwind-protect
          (progn
            (setq ledger-fontify-xact-state-overrides t)
            (insert str)
            (ledger-test-font-lock-fontify-buffer)
            (should (equal (ledger-test-face-groups (buffer-string))
                           face-groups)))
        (setq ledger-fontify-xact-state-overrides nil)))))

;; --------------------------------------------------------------------------------------------------------

(ert-deftest ledger-fontify/test-018 ()
  "Basic format"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03
2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash                 $-20.00
"
   '("2012-03"        ledger-font-posting-date-face
     "2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-uncleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face
     "$-20.00"        ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-019 ()
  "A transaction can have any number of postings"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash                 $-10.00
    Liabilities:Credit          $-10.00
"
   '("2012-03-10"          ledger-font-posting-date-face
     "KFC"                 ledger-font-payee-uncleared-face
     "Expenses:Food"       ledger-font-posting-account-face
     "$20.00"              ledger-font-posting-amount-face
     "Assets:Cash"         ledger-font-posting-account-face
     "$-10.00"             ledger-font-posting-amount-face
     "Liabilities:Credit"  ledger-font-posting-account-face
     "$-10.00"             ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-020 ()
  "Eliding amounts"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash                 $-10.00
    Liabilities:Credit                   ; same as specifying $-10

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash                 $-10.00
    Liabilities:Credit ; semicolon in account name
"
   '("2012-03-10"                 ledger-font-posting-date-face
     "KFC"                        ledger-font-payee-uncleared-face
     "Expenses:Food"              ledger-font-posting-account-face
     "$20.00"                     ledger-font-posting-amount-face
     "Assets:Cash"                ledger-font-posting-account-face
     "$-10.00"                    ledger-font-posting-amount-face
     "Liabilities:Credit"         ledger-font-posting-account-face
     "; same as specifying $-10"  ledger-font-comment-face
     "2012-03-10"                 ledger-font-posting-date-face
     "KFC"                        ledger-font-payee-uncleared-face
     "Expenses:Food"              ledger-font-posting-account-face
     "$20.00"                     ledger-font-posting-amount-face
     "Assets:Cash"                ledger-font-posting-account-face
     "$-10.00"                    ledger-font-posting-amount-face
     "Liabilities:Credit ; semicolon in account name"
                                  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-021 ()
  "Eliding amounts with multiple commodities"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    Expenses:Food                $20.00
    Expenses:Tips                 $2.00
    Assets:Cash              EUR -10.00
    Assets:Cash              GBP -10.00
    Liabilities:Credit
"
   '("2012-03-10"          ledger-font-posting-date-face
     "KFC"                 ledger-font-payee-uncleared-face
     "Expenses:Food"       ledger-font-posting-account-face
     "$20.00"              ledger-font-posting-amount-face
     "Expenses:Tips"       ledger-font-posting-account-face
     "$2.00"               ledger-font-posting-amount-face
     "Assets:Cash"         ledger-font-posting-account-face
     "EUR -10.00"          ledger-font-posting-amount-face
     "Assets:Cash"         ledger-font-posting-account-face
     "GBP -10.00"          ledger-font-posting-amount-face
     "Liabilities:Credit"  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-022 ()
  "Auxiliary dates"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10=2012-03
2012-03-10=2012-03-08 KFC
    Expenses:Food                $20.00
    Assets:Cash                 $-20.00
"
   '("2012-03-10=2012-03"      ledger-font-posting-date-face
     "2012-03-10=2012-03-08"  ledger-font-posting-date-face
     "KFC"                    ledger-font-payee-uncleared-face
     "Expenses:Food"          ledger-font-posting-account-face
     "$20.00"                 ledger-font-posting-amount-face
     "Assets:Cash"            ledger-font-posting-account-face
     "$-20.00"                ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-023 ()
  "Codes"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 (#100)
2012-03-10 (#100) KFC
    Expenses:Food                $20.00
    Assets:Checking
"
   '("2012-03-10"       ledger-font-posting-date-face
     "(#100)"           ledger-font-code-face
     "2012-03-10"            ledger-font-posting-date-face
     "(#100)"                ledger-font-code-face
     "KFC"              ledger-font-payee-uncleared-face
     "Expenses:Food"    ledger-font-posting-account-face
     "$20.00"           ledger-font-posting-amount-face
     "Assets:Checking"  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-024 ()
  "Transaction state cleared"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 *
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("2012-03-10"            ledger-font-posting-date-face
     "2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-cleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-025 ()
  "Transaction state pending"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 !
2012-03-10 ! KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("2012-03-10"            ledger-font-posting-date-face
     "2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-pending-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-026 ()
  "Transaction state cleared at posting level"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    * Expenses:Food                $20.00
    * Assets:Cash

2012-03-10 KFC
    * Expenses:Food                $20.00
    *
"
   '("2012-03-10"       ledger-font-posting-date-face
     "KFC"              ledger-font-payee-uncleared-face
     "* Expenses:Food"  ledger-font-posting-account-cleared-face
     "$20.00"           ledger-font-posting-amount-cleared-face
     "* Assets:Cash"    ledger-font-posting-account-cleared-face
     "2012-03-10"       ledger-font-posting-date-face
     "KFC"              ledger-font-payee-uncleared-face
     "* Expenses:Food"  ledger-font-posting-account-cleared-face
     "$20.00"           ledger-font-posting-amount-cleared-face
     "*"                ledger-font-posting-account-cleared-face)))



(ert-deftest ledger-fontify/test-027 ()
  "Transaction state cleared for one posting"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    Liabilities:Credit            $100.00
    * Assets:Checking
"
   '("2012-03-10"          ledger-font-posting-date-face
     "KFC"                 ledger-font-payee-uncleared-face
     "Liabilities:Credit"  ledger-font-posting-account-face
     "$100.00"             ledger-font-posting-amount-face
     "* Assets:Checking"   ledger-font-posting-account-cleared-face)))



(ert-deftest ledger-fontify/test-028 ()
  "Transaction notes #1"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10  ; yum, chicken...
2012-03-10   ; yum, chicken...
2012-03-10 *  ; yum, chicken...
2012-03-10 * KFC ; yum, chicken...
2012-03-10 * KFC                ; yum, chicken...
    Expenses:Food                $20.00
    Assets:Cash
"
   '("2012-03-10"             ledger-font-posting-date-face
     "; yum, chicken..."      ledger-font-payee-uncleared-face
     "2012-03-10"             ledger-font-posting-date-face
     "; yum, chicken..."      ledger-font-payee-uncleared-face
     "2012-03-10"             ledger-font-posting-date-face
     "; yum, chicken..."      ledger-font-payee-cleared-face
     "2012-03-10"             ledger-font-posting-date-face
     "KFC ; yum, chicken..."  ledger-font-payee-cleared-face
     "2012-03-10"             ledger-font-posting-date-face
     "KFC"                    ledger-font-payee-cleared-face
     "; yum, chicken..."      ledger-font-comment-face
     "Expenses:Food"          ledger-font-posting-account-face
     "$20.00"                 ledger-font-posting-amount-face
     "Assets:Cash"            ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-029 ()
  "Transaction notes #2"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC                ; yum, chicken...
    ; and more notes...
    Expenses:Food                $20.00
    Assets:Cash
"
   '("2012-03-10"           ledger-font-posting-date-face
     "KFC"                  ledger-font-payee-cleared-face
     "; yum, chicken..."    ledger-font-comment-face
     "; and more notes..."  ledger-font-comment-face
     "Expenses:Food"        ledger-font-posting-account-face
     "$20.00"               ledger-font-posting-amount-face
     "Assets:Cash"          ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-030 ()
  "Transaction notes #3"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    ; just these notes...
    Expenses:Food                $20.00
    Assets:Cash
"
   '("2012-03-10"             ledger-font-posting-date-face
     "KFC"                    ledger-font-payee-cleared-face
     "; just these notes..."  ledger-font-comment-face
     "Expenses:Food"          ledger-font-posting-account-face
     "$20.00"                 ledger-font-posting-amount-face
     "Assets:Cash"            ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-031 ()
  "Transaction notes #4"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00  ; posting #1 note
    Assets:Cash
      ; posting #2 note, extra indentation is optional

2012-03-10 * KFC
    Expenses:Food                $20.00 ; posting #1 note
    Assets:Cash
"
   '("2012-03-10"                                        ledger-font-posting-date-face
     "KFC"                                               ledger-font-payee-cleared-face
     "Expenses:Food"                                     ledger-font-posting-account-face
     "$20.00"                                            ledger-font-posting-amount-face
     "; posting #1 note"                                 ledger-font-comment-face
     "Assets:Cash"                                       ledger-font-posting-account-face
     "; posting #2 note, extra indentation is optional"  ledger-font-comment-face
     "2012-03-10"                                        ledger-font-posting-date-face
     "KFC"                                               ledger-font-payee-cleared-face
     "Expenses:Food"                                     ledger-font-posting-account-face
     "$20.00"                                            ledger-font-posting-amount-face
     "; posting #1 note"                                 ledger-font-comment-face
     "Assets:Cash"                                       ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-032 ()
  "Metadata tag"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
      ; :TAG:
"
   '("2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-cleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face
     "; :TAG:"        ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-033 ()
  "Metadata multiple tags"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
      ; :TAG1:TAG2:TAG3:
"
   '("2012-03-10"          ledger-font-posting-date-face
     "KFC"                 ledger-font-payee-cleared-face
     "Expenses:Food"       ledger-font-posting-account-face
     "$20.00"              ledger-font-posting-amount-face
     "Assets:Cash"         ledger-font-posting-account-face
     "; :TAG1:TAG2:TAG3:"  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-034 ()
  "Payee metadata tag"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2010-06-17 Sample
    Assets:Bank        $400.00
    Income:Check1     $-100.00  ; Payee: Person One
    Income:Check2     $-100.00  ; Payee: Person Two
    Income:Check3     $-100.00  ; Payee: Person Three
    Income:Check4     $-100.00  ; Payee: Person Four
"
   '("2010-06-17"             ledger-font-posting-date-face
     "Sample"                 ledger-font-payee-uncleared-face
     "Assets:Bank"            ledger-font-posting-account-face
     "$400.00"                ledger-font-posting-amount-face
     "Income:Check1"          ledger-font-posting-account-face
     "$-100.00"               ledger-font-posting-amount-face
     "; Payee: Person One"    ledger-font-comment-face
     "Income:Check2"          ledger-font-posting-account-face
     "$-100.00"               ledger-font-posting-amount-face
     "; Payee: Person Two"    ledger-font-comment-face
     "Income:Check3"          ledger-font-posting-account-face
     "$-100.00"               ledger-font-posting-amount-face
     "; Payee: Person Three"  ledger-font-comment-face
     "Income:Check4"          ledger-font-posting-account-face
     "$-100.00"               ledger-font-posting-amount-face
     "; Payee: Person Four"   ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-035 ()
  "Metadata values"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
      ; MyTag: This is just a bogus value for MyTag
"
   '("2012-03-10"                                     ledger-font-posting-date-face
     "KFC"                                            ledger-font-payee-cleared-face
     "Expenses:Food"                                  ledger-font-posting-account-face
     "$20.00"                                         ledger-font-posting-amount-face
     "Assets:Cash"                                    ledger-font-posting-account-face
     "; MyTag: This is just a bogus value for MyTag"  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-036 ()
  "Typed metadata: text"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
      ; AuxDate: 2012/02/30
"
   '("2012-03-10"             ledger-font-posting-date-face
     "KFC"                    ledger-font-payee-cleared-face
     "Expenses:Food"          ledger-font-posting-account-face
     "$20.00"                 ledger-font-posting-amount-face
     "Assets:Cash"            ledger-font-posting-account-face
     "; AuxDate: 2012/02/30"  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-037 ()
  "Typed metadata: date"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
      ; AuxDate:: [2012/02/30]
"
   '("2012-03-10"                ledger-font-posting-date-face
     "KFC"                       ledger-font-payee-cleared-face
     "Expenses:Food"             ledger-font-posting-account-face
     "$20.00"                    ledger-font-posting-amount-face
     "Assets:Cash"               ledger-font-posting-account-face
     "; AuxDate:: [2012/02/30]"  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-038 ()
  "Virtual postings"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
    (Budget:Food)               $-20.00
"
   '("2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-cleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face
     "(Budget:Food)"  ledger-font-posting-account-face
     "$-20.00"        ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-039 ()
  "Virtual postings that balance"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food                $20.00
    Assets:Cash
    [Budget:Food]               $-20.00
    [Equity:Budgets]             $20.00
"
   '("2012-03-10"        ledger-font-posting-date-face
     "KFC"               ledger-font-payee-cleared-face
     "Expenses:Food"     ledger-font-posting-account-face
     "$20.00"            ledger-font-posting-amount-face
     "Assets:Cash"       ledger-font-posting-account-face
     "[Budget:Food]"     ledger-font-posting-account-face
     "$-20.00"           ledger-font-posting-amount-face
     "[Equity:Budgets]"  ledger-font-posting-account-face
     "$20.00"            ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-040 ()
  "Expression amounts"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 * KFC
    Expenses:Food      ($10.00 + $20.00)  ; Ledger adds it up for you
    Assets:Cash
"
   '("2012-03-10"                   ledger-font-posting-date-face
     "KFC"                          ledger-font-payee-cleared-face
     "Expenses:Food"                ledger-font-posting-account-face
     "($10.00 + $20.00)"            ledger-font-posting-amount-face
     "; Ledger adds it up for you"  ledger-font-comment-face
     "Assets:Cash"                  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-041 ()
  "Balance assertions"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash                 $-20.00 = $500.00
"
   '("2012-03-10"         ledger-font-posting-date-face
     "KFC"                ledger-font-payee-uncleared-face
     "Expenses:Food"      ledger-font-posting-account-face
     "$20.00"             ledger-font-posting-amount-face
     "Assets:Cash"        ledger-font-posting-account-face
     "$-20.00 = $500.00"  ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-042 ()
  "Balance assignments"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash                         = $500.00
"
   '("2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-uncleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face
     "= $500.00"      ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-043 ()
  "Resetting a balance"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 Adjustment
    Assets:Cash                         = $500.00
    Equity:Adjustments
"
   '("2012-03-10"          ledger-font-posting-date-face
     "Adjustment"          ledger-font-payee-uncleared-face
     "Assets:Cash"         ledger-font-posting-account-face
     "= $500.00"           ledger-font-posting-amount-face
     "Equity:Adjustments"  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-044 ()
  "Balancing transactions"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    [Assets:Brokerage]            = 10 AAPL
"
   '("2012-03-10"          ledger-font-posting-date-face
     "My Broker"           ledger-font-payee-uncleared-face
     "[Assets:Brokerage]"  ledger-font-posting-account-face
     "= 10 AAPL"           ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-045 ()
  "Posting cost"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    Assets:Brokerage             10 AAPL
    Assets:Brokerage:Cash       $-500.00
"
   '("2012-03-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL"                ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$-500.00"               ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-046 ()
  "Explicit posting costs"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    Assets:Brokerage             10 AAPL @ $50.00
    Assets:Brokerage:Cash       $-500.00
"
   '("2012-03-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL @ $50.00"       ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$-500.00"               ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-047 ()
  "Explicit posting costs: elided total"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    Assets:Brokerage             10 AAPL @ $50.00
    Assets:Brokerage:Cash
"
   '("2012-03-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL @ $50.00"       ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-048 ()
  "Posting cost expressions"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    Assets:Brokerage             10 AAPL @ ($500.00 / 10)
    Assets:Brokerage:Cash
"
   '("2012-03-10"                ledger-font-posting-date-face
     "My Broker"                 ledger-font-payee-uncleared-face
     "Assets:Brokerage"          ledger-font-posting-account-face
     "10 AAPL @ ($500.00 / 10)"  ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"     ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-049 ()
  "Posting cost expressions: both"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    Assets:Brokerage             (5 AAPL * 2) @ ($500.00 / 10)
    Assets:Brokerage:Cash
"
   '("2012-03-10"                     ledger-font-posting-date-face
     "My Broker"                      ledger-font-payee-uncleared-face
     "Assets:Brokerage"               ledger-font-posting-account-face
     "(5 AAPL * 2) @ ($500.00 / 10)"  ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"          ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-050 ()
  "Total posting costs"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Broker
    Assets:Brokerage             10 AAPL @@ $500.00
    Assets:Brokerage:Cash
"
   '("2012-03-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL @@ $500.00"     ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-051 ()
  "Virtual posting costs"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-03-10 My Brother
    Assets:Brokerage            1000 AAPL (@) $1
    Income:Gifts Received
"
   '("2012-03-10"             ledger-font-posting-date-face
     "My Brother"             ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "1000 AAPL (@) $1"       ledger-font-posting-amount-face
     "Income:Gifts Received"  ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-052 ()
  "Commodity prices"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage:Cash       $750.00
    Assets:Brokerage            -10 AAPL {$50.00} @ $75.00
"
   '("2012-04-10"                  ledger-font-posting-date-face
     "My Broker"                   ledger-font-payee-uncleared-face
     "Assets:Brokerage:Cash"       ledger-font-posting-account-face
     "$750.00"                     ledger-font-posting-amount-face
     "Assets:Brokerage"            ledger-font-posting-account-face
     "-10 AAPL {$50.00} @ $75.00"  ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-053 ()
  "Total commodity prices"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage:Cash       $750.00
    Assets:Brokerage            -10 AAPL {{$500.00}} @@ $750.00
    Income:Capital Gains       $-250.00
"
   '("2012-04-10"                       ledger-font-posting-date-face
     "My Broker"                        ledger-font-payee-uncleared-face
     "Assets:Brokerage:Cash"            ledger-font-posting-account-face
     "$750.00"                          ledger-font-posting-amount-face
     "Assets:Brokerage"                 ledger-font-posting-account-face
     "-10 AAPL {{$500.00}} @@ $750.00"  ledger-font-posting-amount-face
     "Income:Capital Gains"             ledger-font-posting-account-face
     "$-250.00"                         ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-054 ()
  "Prices versus costs"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage            10 AAPL {$50.00}
    Assets:Brokerage:Cash       $750.00
"
   '("2012-04-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL {$50.00}"       ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$750.00"                ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-055 ()
  "Fixated prices and costs #1"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage            10 AAPL {=$50.00}
    Assets:Brokerage:Cash       $750.00
"
   '("2012-04-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL {=$50.00}"      ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$750.00"                ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-056 ()
  "Fixated prices and costs #2"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage            10 AAPL @ =$50.00
    Assets:Brokerage:Cash       $750.00
"
   '("2012-04-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "10 AAPL @ =$50.00"      ledger-font-posting-amount-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$750.00"                ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-057 ()
  "Lot dates"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage:Cash       $375.00
    Assets:Brokerage            -5 AAPL {$50.00} [2012-04-10] @ $375.00
    Income:Capital Gains       $-125.00
"
   '("2012-04-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$375.00"                ledger-font-posting-amount-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "-5 AAPL {$50.00} [2012-04-10] @ $375.00"
                              ledger-font-posting-amount-face
     "Income:Capital Gains"   ledger-font-posting-account-face
     "$-125.00"               ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-058 ()
  "Lot notes"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    Assets:Brokerage:Cash       $375.00
    Assets:Brokerage            -5 AAPL {$50.00} [2012-04-10] (Oh my!) @ $375.00
    Income:Capital Gains       $-125.00
"
   '("2012-04-10"             ledger-font-posting-date-face
     "My Broker"              ledger-font-payee-uncleared-face
     "Assets:Brokerage:Cash"  ledger-font-posting-account-face
     "$375.00"                ledger-font-posting-amount-face
     "Assets:Brokerage"       ledger-font-posting-account-face
     "-5 AAPL {$50.00} [2012-04-10] (Oh my!) @ $375.00"
                              ledger-font-posting-amount-face
     "Income:Capital Gains"   ledger-font-posting-account-face
     "$-125.00"               ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-059 ()
  "Lot value expressions #1"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
define ten_dollars(s, date, t) = market($10, date, t)

2012-04-10 My Broker
    Assets:Brokerage:Cash       $375.00
    Assets:Brokerage            -5 AAPL {$50.00} ((ten_dollars)) @@ $375.00
    Income:Capital Gains       $-125.00
"
   '("define"                   ledger-font-define-directive-face
     "ten_dollars(s, date, t)"  ledger-font-define-name-face
     "market($10, date, t)"     ledger-font-define-body-face
     "2012-04-10"               ledger-font-posting-date-face
     "My Broker"                ledger-font-payee-uncleared-face
     "Assets:Brokerage:Cash"    ledger-font-posting-account-face
     "$375.00"                  ledger-font-posting-amount-face
     "Assets:Brokerage"         ledger-font-posting-account-face
     "-5 AAPL {$50.00} ((ten_dollars)) @@ $375.00"
                                ledger-font-posting-amount-face
     "Income:Capital Gains"     ledger-font-posting-account-face
     "$-125.00"                 ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-060 ()
  "Lot value expressions #2"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2012-04-10 My Broker
    A:B:Cash       $375.00
    A:B     -5 AAPL {$50.00} ((s, d, t -> market($10, date, t))) @@ $375.00
    Income:Capital Gains       $-125.00
"
   '("2012-04-10"            ledger-font-posting-date-face
     "My Broker"             ledger-font-payee-uncleared-face
     "A:B:Cash"              ledger-font-posting-account-face
     "$375.00"               ledger-font-posting-amount-face
     "A:B"                   ledger-font-posting-account-face
     "-5 AAPL {$50.00} ((s, d, t -> market($10, date, t))) @@ $375.00"
                             ledger-font-posting-amount-face
     "Income:Capital Gains"  ledger-font-posting-account-face
     "$-125.00"              ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-061 ()
  "Automated Transactions"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= expr true
    Foo                          $50.00
    Bar                         $-50.00

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("= expr true"    ledger-font-auto-xact-face
     "Foo"            ledger-font-posting-account-face
     "$50.00"         ledger-font-posting-amount-face
     "Bar"            ledger-font-posting-account-face
     "$-50.00"        ledger-font-posting-amount-face
     "2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-uncleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-062 ()
  "Amount multipliers"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= expr true
    Foo                           50.00
    Bar                          -50.00

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("= expr true"    ledger-font-auto-xact-face
     "Foo"            ledger-font-posting-account-face
     "50.00"          ledger-font-posting-amount-face
     "Bar"            ledger-font-posting-account-face
     "-50.00"         ledger-font-posting-amount-face
     "2012-03-10"     ledger-font-posting-date-face
     "KFC"            ledger-font-payee-uncleared-face
     "Expenses:Food"  ledger-font-posting-account-face
     "$20.00"         ledger-font-posting-amount-face
     "Assets:Cash"    ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-063 ()
  "Accessing the matching posting’s amount"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= expr true
    (Foo)                  (amount * 2)  ; same as just 2 in this case

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("= expr true"                    ledger-font-auto-xact-face
     "(Foo)"                          ledger-font-posting-account-face
     "(amount * 2)"                   ledger-font-posting-amount-face
     "; same as just 2 in this case"  ledger-font-comment-face
     "2012-03-10"                     ledger-font-posting-date-face
     "KFC"                            ledger-font-payee-uncleared-face
     "Expenses:Food"                  ledger-font-posting-account-face
     "$20.00"                         ledger-font-posting-amount-face
     "Assets:Cash"                    ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-064 ()
  "Referring to the matching posting’s account"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= food
    (Budget:$account)                10

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("= food"             ledger-font-auto-xact-face
     "(Budget:$account)"  ledger-font-posting-account-face
     "10"                 ledger-font-posting-amount-face
     "2012-03-10"         ledger-font-posting-date-face
     "KFC"                ledger-font-payee-uncleared-face
     "Expenses:Food"      ledger-font-posting-account-face
     "$20.00"             ledger-font-posting-amount-face
     "Assets:Cash"        ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-065 ()
  "Applying metadata to every matched posting"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= food
    ; Foo: Bar
    (Budget:$account)                10

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("= food"             ledger-font-auto-xact-face
     "; Foo: Bar"         ledger-font-comment-face
     "(Budget:$account)"  ledger-font-posting-account-face
     "10"                 ledger-font-posting-amount-face
     "2012-03-10"         ledger-font-posting-date-face
     "KFC"                ledger-font-payee-uncleared-face
     "Expenses:Food"      ledger-font-posting-account-face
     "$20.00"             ledger-font-posting-amount-face
     "Assets:Cash"        ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-066 ()
  "Applying metadata to the generated posting"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= food
    (Budget:$account)                10
      ; Foo: Bar

2012-03-10 KFC
    Expenses:Food                $20.00
    Assets:Cash
"
   '("= food"             ledger-font-auto-xact-face
     "(Budget:$account)"  ledger-font-posting-account-face
     "10"                 ledger-font-posting-amount-face
     "; Foo: Bar"         ledger-font-comment-face
     "2012-03-10"         ledger-font-posting-date-face
     "KFC"                ledger-font-payee-uncleared-face
     "Expenses:Food"      ledger-font-posting-account-face
     "$20.00"             ledger-font-posting-amount-face
     "Assets:Cash"        ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-067 ()
  "Effective Dates on postings"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
2008/10/16 * (2090) Bountiful Blessings Farm
    Expenses:Food:Groceries                  $ 37.50  ; [=2008/10/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2008/11/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2008/12/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2009/01/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2009/02/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2009/03/01]
    Assets:Checking
"
   '("2008/10/16"                ledger-font-posting-date-face
     "(2090)"                    ledger-font-code-face
     "Bountiful Blessings Farm"  ledger-font-payee-cleared-face
     "Expenses:Food:Groceries"   ledger-font-posting-account-face
     "$ 37.50"                   ledger-font-posting-amount-face
     "; [=2008/10/01]"           ledger-font-comment-face
     "Expenses:Food:Groceries"   ledger-font-posting-account-face
     "$ 37.50"                   ledger-font-posting-amount-face
     "; [=2008/11/01]"           ledger-font-comment-face
     "Expenses:Food:Groceries"   ledger-font-posting-account-face
     "$ 37.50"                   ledger-font-posting-amount-face
     "; [=2008/12/01]"           ledger-font-comment-face
     "Expenses:Food:Groceries"   ledger-font-posting-account-face
     "$ 37.50"                   ledger-font-posting-amount-face
     "; [=2009/01/01]"           ledger-font-comment-face
     "Expenses:Food:Groceries"   ledger-font-posting-account-face
     "$ 37.50"                   ledger-font-posting-amount-face
     "; [=2009/02/01]"           ledger-font-comment-face
     "Expenses:Food:Groceries"   ledger-font-posting-account-face
     "$ 37.50"                   ledger-font-posting-amount-face
     "; [=2009/03/01]"           ledger-font-comment-face
     "Assets:Checking"           ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-068 ()
  "Periodic Transactions"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
~ Monthly
    Expenses:Rent               $500.00
    Expenses:Food               $450.00
    Expenses:Auto:Gas           $120.00
    Expenses:Insurance          $150.00
    Expenses:Phone              $125.00
    Expenses:Utilities          $100.00
    Expenses:Movies              $50.00
    Expenses                    $200.00  ; all other expenses
    Assets

~ Yearly
    Expenses:Auto:Repair        $500.00
    Assets
"
   '("~ Monthly"             ledger-font-periodic-xact-face
     "Expenses:Rent"         ledger-font-posting-account-face
     "$500.00"               ledger-font-posting-amount-face
     "Expenses:Food"         ledger-font-posting-account-face
     "$450.00"               ledger-font-posting-amount-face
     "Expenses:Auto:Gas"     ledger-font-posting-account-face
     "$120.00"               ledger-font-posting-amount-face
     "Expenses:Insurance"    ledger-font-posting-account-face
     "$150.00"               ledger-font-posting-amount-face
     "Expenses:Phone"        ledger-font-posting-account-face
     "$125.00"               ledger-font-posting-amount-face
     "Expenses:Utilities"    ledger-font-posting-account-face
     "$100.00"               ledger-font-posting-amount-face
     "Expenses:Movies"       ledger-font-posting-account-face
     "$50.00"                ledger-font-posting-amount-face
     "Expenses"              ledger-font-posting-account-face
     "$200.00"               ledger-font-posting-amount-face
     "; all other expenses"  ledger-font-comment-face
     "Assets"                ledger-font-posting-account-face
     "~ Yearly"              ledger-font-periodic-xact-face
     "Expenses:Auto:Repair"  ledger-font-posting-account-face
     "$500.00"               ledger-font-posting-amount-face
     "Assets"                ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-069 ()
  "Concrete Example of Automated Transactions"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
= /^(?:Income:|Expenses:(?:Business|Rent$|Furnishings|Taxes|Insurance))/
  (Liabilities:Huququ'llah)               0.19

2003/01/01 (99) Salary
  Income:Salary  -$1000
  Assets:Checking
"
   '("= /^(?:Income:|Expenses:(?:Business|Rent$|Furnishings|Taxes|Insurance))/"
                                  ledger-font-auto-xact-face
     "(Liabilities:Huququ'llah)"  ledger-font-posting-account-face
     "0.19"                       ledger-font-posting-amount-face
     "2003/01/01"                 ledger-font-posting-date-face
     "(99)"                       ledger-font-code-face
     "Salary"                     ledger-font-payee-uncleared-face
     "Income:Salary"              ledger-font-posting-account-face
     "-$1000"                     ledger-font-posting-amount-face
     "Assets:Checking"            ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-070 ()
  "Commenting on your Journal"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
; This is a single line comment,
#  and this,
%   and this,
|    and this,
*     and this.
"
   '("; This is a single line comment," ledger-font-comment-face
     "#  and this,"     ledger-font-comment-face
     "%   and this,"    ledger-font-comment-face
     "|    and this,"   ledger-font-comment-face
     "*     and this."  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-071 ()
  "Commodity names with white-space or numeric characters"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
1999/06/09 ! Achat
    Actif:SG PEE STK         49.957 \"Arcancia Équilibre 454\"
    Actif:SG PEE STK      $-234.90
"
   '("1999/06/09"                         ledger-font-posting-date-face
     "Achat"                              ledger-font-payee-pending-face
     "Actif:SG PEE STK"                   ledger-font-posting-account-face
     "49.957 \"Arcancia Équilibre 454\""  ledger-font-posting-amount-face
     "Actif:SG PEE STK"                   ledger-font-posting-account-face
     "$-234.90"                           ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-072 ()
  "Command Directives: account"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
account
account Expenses:Food
    note
    alias
    payee
    check
    assert
    eval
account Expenses:Food
    note This account is all about the chicken!
    alias food
    payee ^(KFC|Popeyes)$
    check commodity == \"$\"
    assert commodity == \"$\"
    eval print(\"Hello!\")
    default

"
   '("account"                                 ledger-font-account-directive-face
     "account"                                 ledger-font-account-directive-face
     "Expenses:Food"                           ledger-font-account-name-face
     "note"                                    ledger-font-note-directive-face
     "alias"                                   ledger-font-alias-directive-face
     "payee"                                   ledger-font-payee-directive-face
     "check"                                   ledger-font-check-directive-face
     "assert"                                  ledger-font-assert-directive-face
     "eval"                                    ledger-font-expr-directive-face
     "account"                                 ledger-font-account-directive-face
     "Expenses:Food"                           ledger-font-account-name-face
     "note"                                    ledger-font-note-directive-face
     "This account is all about the chicken!"  ledger-font-note-text-face
     "alias"                                   ledger-font-alias-directive-face
     "food"                                    ledger-font-account-name-face
     "payee"                                   ledger-font-payee-directive-face
     "^(KFC|Popeyes)$"                         ledger-font-payee-regex-face
     "check"                                   ledger-font-check-directive-face
     "commodity == \"$\""                      ledger-font-check-condition-face
     "assert"                                  ledger-font-assert-directive-face
     "commodity == \"$\""                      ledger-font-assert-condition-face
     "eval"                                    ledger-font-expr-directive-face
     "print(\"Hello!\")"                       ledger-font-expr-expression-face
     "default"                                 ledger-font-default-directive-face)))



(ert-deftest ledger-fontify/test-073 ()
  "Command Directives: apply account"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
apply
apply account
apply account Personal
2011/11/15  Supermarket
    Expenses:Groceries      $ 50.00
    Assets:Checking
end apply account

"
   '(
     "apply"               ledger-font-apply-directive-face
     "apply"               ledger-font-apply-directive-face
     "account"             ledger-font-apply-directive-face
     "apply"               ledger-font-apply-directive-face
     "account"             ledger-font-apply-directive-face
     "Personal"            ledger-font-apply-account-face
     "2011/11/15"          ledger-font-posting-date-face
     "Supermarket"         ledger-font-payee-uncleared-face
     "Expenses:Groceries"  ledger-font-posting-account-face
     "$ 50.00"             ledger-font-posting-amount-face
     "Assets:Checking"     ledger-font-posting-account-face
     "end"                 ledger-font-end-directive-face
     "apply"               ledger-font-end-directive-face
     "account"             ledger-font-end-directive-face)))



(ert-deftest ledger-fontify/test-074 ()
  "Command Directives: alias"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
alias
alias Entertainment
alias Entertainment=
alias Entertainment=Expenses:Entertainment
alias Dining=Entertainment:Dining
alias Checking=Assets:Credit Union:Joint Checking Account

2011/11/30 ChopChop
  Dining          $10.00
  Checking

"
   '("alias"                                       ledger-font-alias-directive-face
     "alias"                                       ledger-font-alias-directive-face
     "Entertainment"                               ledger-font-account-name-face
     "alias"                                       ledger-font-alias-directive-face
     "Entertainment"                               ledger-font-account-name-face
     "alias"                                       ledger-font-alias-directive-face
     "Entertainment"                               ledger-font-account-name-face
     "Expenses:Entertainment"                      ledger-font-alias-definition-face
     "alias"                                       ledger-font-alias-directive-face
     "Dining"                                      ledger-font-account-name-face
     "Entertainment:Dining"                        ledger-font-alias-definition-face
     "alias"                                       ledger-font-alias-directive-face
     "Checking"                                    ledger-font-account-name-face
     "Assets:Credit Union:Joint Checking Account"  ledger-font-alias-definition-face
     "2011/11/30"                                  ledger-font-posting-date-face
     "ChopChop"                                    ledger-font-payee-uncleared-face
     "Dining"                                      ledger-font-posting-account-face
     "$10.00"                                      ledger-font-posting-amount-face
     "Checking"                                    ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-075 ()
  "Command Directives: assert"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
assert
assert commodity == 'VIFSX'
assert comment =~ /REGEX/
"
   '("assert"                ledger-font-assert-directive-face
     "assert"                ledger-font-assert-directive-face
     "commodity == 'VIFSX'"  ledger-font-assert-condition-face
     "assert"                ledger-font-assert-directive-face
     "comment =~ /REGEX/"    ledger-font-assert-condition-face)))



(ert-deftest ledger-fontify/test-076 ()
  "Command Directives: bucket"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
bucket
bucket Assets:Checking
2011/01/25 Tom's Used Cars
    Expenses:Auto                    $ 5,500.00

2011/01/27 Book Store
    Expenses:Books                       $20.00

2011/12/01 Sale
    Assets:Checking:Business            $ 30.00
"
   '("bucket"                    ledger-font-bucket-directive-face
     "bucket"                    ledger-font-bucket-directive-face
     "Assets:Checking"           ledger-font-bucket-account-face
     "2011/01/25"                ledger-font-posting-date-face
     "Tom's Used Cars"           ledger-font-payee-uncleared-face
     "Expenses:Auto"             ledger-font-posting-account-face
     "$ 5,500.00"                ledger-font-posting-amount-face
     "2011/01/27"                ledger-font-posting-date-face
     "Book Store"                ledger-font-payee-uncleared-face
     "Expenses:Books"            ledger-font-posting-account-face
     "$20.00"                    ledger-font-posting-amount-face
     "2011/12/01"                ledger-font-posting-date-face
     "Sale"                      ledger-font-payee-uncleared-face
     "Assets:Checking:Business"  ledger-font-posting-account-face
     "$ 30.00"                   ledger-font-posting-amount-face)))



(ert-deftest ledger-fontify/test-077 ()
  "Command Directives: capture"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
capture
capture Expenses:Deductible:Medical
capture  Expenses:Deductible:Medical  Medical
"
   '("capture"                      ledger-font-capture-directive-face
     "capture"                      ledger-font-capture-directive-face
     "Expenses:Deductible:Medical"  ledger-font-capture-account-face
     "capture"                      ledger-font-capture-directive-face
     "Expenses:Deductible:Medical"  ledger-font-capture-account-face
     "Medical"                      ledger-font-capture-regex-face)))



(ert-deftest ledger-fontify/test-078 ()
  "Command Directives: check"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
check
check amount == $20
check account =~ /REGEX/
"
   '("check"               ledger-font-check-directive-face
     "check"               ledger-font-check-directive-face
     "amount == $20"       ledger-font-check-condition-face
     "check"               ledger-font-check-directive-face
     "account =~ /REGEX/"  ledger-font-check-condition-face)))



(ert-deftest ledger-fontify/test-079 ()
  "Command Directives: comment"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
comment
    This is a block comment with
    multiple lines
end comment
"
   '("comment
    This is a block comment with
    multiple lines
end comment
"  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-080 ()
  "Command Directives: commodity"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
commodity
commodity $
    note
    format
commodity $
   note American Dollars
   format $1,000.00
   nomarket
   default

"
   '("commodity"         ledger-font-commodity-directive-face
     "commodity"         ledger-font-commodity-directive-face
     "$"                 ledger-font-commodity-name-face
     "note"              ledger-font-note-directive-face
     "format"            ledger-font-format-directive-face
     "commodity"         ledger-font-commodity-directive-face
     "$"                 ledger-font-commodity-name-face
     "note"              ledger-font-note-directive-face
     "American Dollars"  ledger-font-note-text-face
     "format"            ledger-font-format-directive-face
     "$1,000.00"         ledger-font-commodity-format-face
     "nomarket"          ledger-font-N-directive-face
     "default"           ledger-font-default-directive-face)))



(ert-deftest ledger-fontify/test-081 ()
  "Command Directives: define"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
define
define var_name
define var_name=
define var_name=$100

2011/12/01 Test
    Expenses  (var_name*4)
    Assets
"
   '("define"          ledger-font-define-directive-face
     "define"          ledger-font-define-directive-face
     "var_name"        ledger-font-define-name-face
     "define"          ledger-font-define-directive-face
     "var_name"        ledger-font-define-name-face
     "define"          ledger-font-define-directive-face
     "var_name"        ledger-font-define-name-face
     "$100"            ledger-font-define-body-face
     "2011/12/01"      ledger-font-posting-date-face
     "Test"            ledger-font-payee-uncleared-face
     "Expenses"  ledger-font-posting-account-face
     "(var_name*4)"    ledger-font-posting-amount-face
     "Assets"      ledger-font-posting-account-face)))



(ert-deftest ledger-fontify/test-082 ()
  "Command Directives: end"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
end
end apply
end apply account
end apply tag
"
   '("end"      ledger-font-end-directive-face
     "end"      ledger-font-end-directive-face
     "apply"    ledger-font-end-directive-face
     "end"      ledger-font-end-directive-face
     "apply"    ledger-font-end-directive-face
     "account"  ledger-font-end-directive-face
     "end"      ledger-font-end-directive-face
     "apply"    ledger-font-end-directive-face
     "tag"      ledger-font-end-directive-face)))



(ert-deftest ledger-fontify/test-083 ()
  "Command Directives: expr"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
expr
expr 2*2
"
   '("expr"  ledger-font-expr-directive-face
     "expr"  ledger-font-expr-directive-face
     "2*2"   ledger-font-expr-expression-face)))



(ert-deftest ledger-fontify/test-084 ()
  "Command Directives: fixed"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
fixed
fixed CAD
fixed CAD $0.90
2012-04-10 Lunch in Canada
    Assets:Wallet            -15.50 CAD
    Expenses:Food            15.50 CAD

2012-04-11 Second day Dinner in Canada
    Assets:Wallet            -25.75 CAD
    Expenses:Food            25.75 CAD
endfixed
endfixed CAD
"
   '("fixed"                        ledger-font-fixed-directive-face
     "fixed"                        ledger-font-fixed-directive-face
     "CAD"                          ledger-font-fixed-commodity-face
     "fixed"                        ledger-font-fixed-directive-face
     "CAD"                          ledger-font-fixed-commodity-face
     "$0.90"                        ledger-font-fixed-price-face
     "2012-04-10"                   ledger-font-posting-date-face
     "Lunch in Canada"              ledger-font-payee-uncleared-face
     "Assets:Wallet"                ledger-font-posting-account-face
     "-15.50 CAD"                   ledger-font-posting-amount-face
     "Expenses:Food"                ledger-font-posting-account-face
     "15.50 CAD"                    ledger-font-posting-amount-face
     "2012-04-11"                   ledger-font-posting-date-face
     "Second day Dinner in Canada"  ledger-font-payee-uncleared-face
     "Assets:Wallet"                ledger-font-posting-account-face
     "-25.75 CAD"                   ledger-font-posting-amount-face
     "Expenses:Food"                ledger-font-posting-account-face
     "25.75 CAD"                    ledger-font-posting-amount-face
     "endfixed"                     ledger-font-end-directive-face
     "endfixed"                     ledger-font-end-directive-face
     "CAD"                          ledger-font-fixed-commodity-face)))



(ert-deftest ledger-fontify/test-085 ()
  "Command Directives: include"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
include
include file.ledger
"
   '("include"  ledger-font-include-directive-face
     "include"  ledger-font-include-directive-face
     "file.ledger" ledger-font-include-filename-face)))



(ert-deftest ledger-fontify/test-086 ()
  "Command Directives: payee"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
payee
payee KFC
    alias
    uuid
payee KFC
    alias KENTUCKY FRIED CHICKEN
    uuid 2a2e21d434356f886c84371eebac6e44f1337fda
"
   '("payee"                                     ledger-font-payee-directive-face
     "payee"                                     ledger-font-payee-directive-face
     "KFC"                                       ledger-font-payee-name-face
     "alias"                                     ledger-font-alias-directive-face
     "uuid"                                      ledger-font-uuid-directive-face
     "payee"                                     ledger-font-payee-directive-face
     "KFC"                                       ledger-font-payee-name-face
     "alias"                                     ledger-font-alias-directive-face
     "KENTUCKY FRIED CHICKEN"                    ledger-font-payee-regex-face
     "uuid"                                      ledger-font-uuid-directive-face
     "2a2e21d434356f886c84371eebac6e44f1337fda"  ledger-font-uuid-face)))



(ert-deftest ledger-fontify/test-087 ()
  "Command Directives: apply tag"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
apply
apply tag
apply tag hastag
apply tag nestedtag: true

2011/01/25 Tom's Used Cars
    Expenses:Auto                    $ 5,500.00
    ; :nobudget:
    Assets:Checking

2011/01/27 Book Store
    Expenses:Books                       $20.00
    Liabilities:MasterCard

end apply tag

2011/12/01 Sale
    Assets:Checking:Business            $ 30.00
    Income:Sales

end apply tag
"
   '("apply"                     ledger-font-apply-directive-face
     "apply"                     ledger-font-apply-directive-face
     "tag"                       ledger-font-apply-directive-face
     "apply"                     ledger-font-apply-directive-face
     "tag"                       ledger-font-apply-directive-face
     "hastag"                    ledger-font-apply-tag-face
     "apply"                     ledger-font-apply-directive-face
     "tag"                       ledger-font-apply-directive-face
     "nestedtag: true"           ledger-font-apply-tag-face
     "2011/01/25"                ledger-font-posting-date-face
     "Tom's Used Cars"           ledger-font-payee-uncleared-face
     "Expenses:Auto"             ledger-font-posting-account-face
     "$ 5,500.00"                ledger-font-posting-amount-face
     "; :nobudget:"              ledger-font-comment-face
     "Assets:Checking"           ledger-font-posting-account-face
     "2011/01/27"                ledger-font-posting-date-face
     "Book Store"                ledger-font-payee-uncleared-face
     "Expenses:Books"            ledger-font-posting-account-face
     "$20.00"                    ledger-font-posting-amount-face
     "Liabilities:MasterCard"    ledger-font-posting-account-face
     "end"                       ledger-font-end-directive-face
     "apply"                     ledger-font-end-directive-face
     "tag"                       ledger-font-end-directive-face
     "2011/12/01"                ledger-font-posting-date-face
     "Sale"                      ledger-font-payee-uncleared-face
     "Assets:Checking:Business"  ledger-font-posting-account-face
     "$ 30.00"                   ledger-font-posting-amount-face
     "Income:Sales"              ledger-font-posting-account-face
     "end"                       ledger-font-end-directive-face
     "apply"                     ledger-font-end-directive-face
     "tag"                       ledger-font-end-directive-face)))



(ert-deftest ledger-fontify/test-088 ()
  "Command Directives: tag"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
tag
tag Receipt
  check
  assert
tag Receipt
  check value =~ /pattern/
  assert value != 'foobar'
"
   '("tag"                 ledger-font-tag-directive-face
     "tag"                 ledger-font-tag-directive-face
     "Receipt"             ledger-font-tag-name-face
     "check"               ledger-font-check-directive-face
     "assert"              ledger-font-assert-directive-face
     "tag"                 ledger-font-tag-directive-face
     "Receipt"             ledger-font-tag-name-face
     "check"               ledger-font-check-directive-face
     "value =~ /pattern/"  ledger-font-check-condition-face
     "assert"              ledger-font-assert-directive-face
     "value != 'foobar'"   ledger-font-assert-condition-face)))



(ert-deftest ledger-fontify/test-089 ()
  "Command Directives: test"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
test reg --now 2014-05-14 -p 'this month'
14-May-13 Bug 1038 Test         Expenses:Some:Account          $500         $500
                                Assets:Cash                   $-500            0
end test
"
   '("test reg --now 2014-05-14 -p 'this month'
14-May-13 Bug 1038 Test         Expenses:Some:Account          $500         $500
                                Assets:Cash                   $-500            0
end test
"  ledger-font-comment-face)))



(ert-deftest ledger-fontify/test-090 ()
  "Command Directives: year"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
year
year 2004
"
   '("year"  ledger-font-year-directive-face
     "year"  ledger-font-year-directive-face
     "2004"  ledger-font-year-face)))



(ert-deftest ledger-fontify/test-091 ()
  "Command Directives: A"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
A Assets:Checking
"
   '("A"                ledger-font-bucket-directive-face
     "Assets:Checking"  ledger-font-bucket-account-face)))



(ert-deftest ledger-fontify/test-092 ()
  "Command Directives: Y"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
Y
Y 2016
"
   '("Y"     ledger-font-year-directive-face
     "Y"     ledger-font-year-directive-face
     "2016"  ledger-font-year-face)))



(ert-deftest ledger-fontify/test-093 ()
  "Command Directives: N"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
N
N SYMBOL
"
   '("N"  ledger-font-N-directive-face
     "N"  ledger-font-N-directive-face
     "SYMBOL" ledger-font-N-symbol-face)))



(ert-deftest ledger-fontify/test-094 ()
  "Command Directives: D"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
D
D $1,000.00
"
   '("D"  ledger-font-D-directive-face
     "D"  ledger-font-D-directive-face
     "$1,000.00" ledger-font-commodity-format-face)))



(ert-deftest ledger-fontify/test-095 ()
  "Command Directives: C"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
C
C 1.00
C 1.00 Kb =
C 1.00 Kb = 1024 bytes
"
   '("C"           ledger-font-C-directive-face
     "C"           ledger-font-C-directive-face
     "1.00"        ledger-font-C-amount-face
     "C"           ledger-font-C-directive-face
     "1.00 Kb"     ledger-font-C-amount-face
     "C"           ledger-font-C-directive-face
     "1.00 Kb"     ledger-font-C-amount-face
     "1024 bytes"  ledger-font-C-amount-face)))



(ert-deftest ledger-fontify/test-096 ()
  "Command Directives: I, i, O, o, b, h"
  :tags '(font baseline)

  (ledger-test-font-lock
   "
h
h 2013/03/28 7 Account
b
b 2013/03/27 3600 Account
I
I 2013/03
I 2013/03/28 17:01
I 2013/03/28 17:01:30 Account
I 2013/03/28 17:01:30 Account  PAYEE
O
O 2013/03
O 2013/03/29 18:39
O 2013/03/29 18:39:00 Account
O 2013/03/29 18:39:00 Account  PAYEE
i
i 2013/03
i 2013/03/28 22:13
i 2013/03/28 22:13:00 Account
i 2013/03/28 22:13:00 Account  PAYEE
o
o 2013/03
o 2013/03/29 03:39
o 2013/03/29 03:39:00 Account
o 2013/03/29 03:39:00 Account  PAYEE
"
   '("h"                    ledger-font-timeclock-directive-face
     "h"                    ledger-font-timeclock-directive-face
     "b"                    ledger-font-timeclock-directive-face
     "b"                    ledger-font-timeclock-directive-face
     "I"                    ledger-font-timeclock-directive-face
     "I"                    ledger-font-timeclock-directive-face
     "2013/03"              ledger-font-posting-date-face
     "I"                    ledger-font-timeclock-directive-face
     "2013/03/28 17:01"     ledger-font-posting-date-face
     "I"                    ledger-font-timeclock-directive-face
     "2013/03/28 17:01:30"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-cleared-face
     "I"                    ledger-font-timeclock-directive-face
     "2013/03/28 17:01:30"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-cleared-face
     "PAYEE"                ledger-font-payee-cleared-face
     "O"                    ledger-font-timeclock-directive-face
     "O"                    ledger-font-timeclock-directive-face
     "2013/03"              ledger-font-posting-date-face
     "O"                    ledger-font-timeclock-directive-face
     "2013/03/29 18:39"     ledger-font-posting-date-face
     "O"                    ledger-font-timeclock-directive-face
     "2013/03/29 18:39:00"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-cleared-face
     "O"                    ledger-font-timeclock-directive-face
     "2013/03/29 18:39:00"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-cleared-face
     "PAYEE"                ledger-font-payee-cleared-face
     "i"                    ledger-font-timeclock-directive-face
     "i"                    ledger-font-timeclock-directive-face
     "2013/03"              ledger-font-posting-date-face
     "i"                    ledger-font-timeclock-directive-face
     "2013/03/28 22:13"     ledger-font-posting-date-face
     "i"                    ledger-font-timeclock-directive-face
     "2013/03/28 22:13:00"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-face
     "i"                    ledger-font-timeclock-directive-face
     "2013/03/28 22:13:00"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-face
     "PAYEE"                ledger-font-payee-uncleared-face
     "o"                    ledger-font-timeclock-directive-face
     "o"                    ledger-font-timeclock-directive-face
     "2013/03"              ledger-font-posting-date-face
     "o"                    ledger-font-timeclock-directive-face
     "2013/03/29 03:39"     ledger-font-posting-date-face
     "o"                    ledger-font-timeclock-directive-face
     "2013/03/29 03:39:00"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-face
     "o"                    ledger-font-timeclock-directive-face
     "2013/03/29 03:39:00"  ledger-font-posting-date-face
     "Account"              ledger-font-posting-account-face
     "PAYEE"                ledger-font-payee-uncleared-face)))


(ert-deftest ledger-fontify/test-097 ()
  "double Y directive"
  :tags '(font regress)

  (ledger-test-font-lock
   "
YY 2015
"
   nil))


(ert-deftest ledger-fontify/test-098 ()
  "space after directive"
  :tags '(font regress)

  (ledger-test-font-lock
   "
payeee Charity
"
   nil))


(ert-deftest ledger-fontify/test-099 ()
  "partial buffer fontification"
  :tags '(font regress)
  (let* ((pre-str "
2014/11/02 * beer
    Expenses:Drinks                                5 EUR
    Assets:Checking                               -5 EUR

2014/11/02=2014/11/03 * salary
    Assets:Checking                               10 EUR")
         (str "
    Income                                       -10 EUR

2014/11/02 * burger
    Expenses:Food                                  7 EUR")
         (post-str "
    Assets:Checking                               -7 EUR
")
         (beg (+ 1 (length pre-str)))
         (end (+ beg (length str)))
         (face-groups
          '("Income"         ledger-font-posting-account-face
            "-10 EUR"        ledger-font-posting-amount-face
            "2014/11/02"     ledger-font-posting-date-face
            "burger"         ledger-font-payee-cleared-face
            "Expenses:Food"  ledger-font-posting-account-face
            "7 EUR"          ledger-font-posting-amount-face)))
    (with-temp-buffer
      (ledger-mode)
      (insert pre-str str post-str)
      (font-lock-fontify-region beg end nil)
      (should (equal (ledger-test-face-groups (buffer-substring beg end))
                     face-groups)))))


(ert-deftest ledger-fontify/test-100 ()
  "price with date and time"
  :tags '(font regress)

  (ledger-test-font-lock
   "
P
P 2004/06
P 2004/06/21 02:17
P 2004/06/21 02:17:58 TWCUX
P 2004/06/21 02:17:58 TWCUX $27.76
P 2004/06/21 02:17:59 AGTHX $25.41
P 2004/06/21 02:18:00 OPTFX $39.31
P 2004/06/21 02:18:01 FEQTX $22.49
P 2004/06/21 02:18:02 AAPL $32.91
"
   '("P"                    ledger-font-price-directive-face
     "P"                    ledger-font-price-directive-face
     "2004/06"              ledger-font-price-date-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:17"     ledger-font-price-date-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:17:58"  ledger-font-price-date-face
     "TWCUX"                ledger-font-price-symbol-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:17:58"  ledger-font-price-date-face
     "TWCUX"                ledger-font-price-symbol-face
     "$27.76"               ledger-font-price-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:17:59"  ledger-font-price-date-face
     "AGTHX"                ledger-font-price-symbol-face
     "$25.41"               ledger-font-price-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:18:00"  ledger-font-price-date-face
     "OPTFX"                ledger-font-price-symbol-face
     "$39.31"               ledger-font-price-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:18:01"  ledger-font-price-date-face
     "FEQTX"                ledger-font-price-symbol-face
     "$22.49"               ledger-font-price-face
     "P"                    ledger-font-price-directive-face
     "2004/06/21 02:18:02"  ledger-font-price-date-face
     "AAPL"                 ledger-font-price-symbol-face
     "$32.91"               ledger-font-price-face)))


(provide 'fontify-test)

;;; fontify-test.el ends here
