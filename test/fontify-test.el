;;; fontify-test.el --- ERT for ledger-mode

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
   '("2010/12/01"                  ledger-font-posting-date-face
     " Checking balance"           ledger-font-payee-cleared-face
     "  Assets:Checking  "         ledger-font-posting-account-face
     "                 $1,000.00"  ledger-font-posting-amount-face
     "  Equity:Opening Balances"   ledger-font-posting-account-face)))


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
   '("2010/12/01"                 ledger-font-posting-date-face
     " Checking balance"          ledger-font-payee-cleared-face
     "  Assets:Checking  "        ledger-font-posting-account-face
     "                 $42.00 "   ledger-font-posting-amount-face
     "; the answer to life"       ledger-font-comment-face
     "  Equity:Opening Balances"  ledger-font-posting-account-face)))


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
   '("2010/12/01"                   ledger-font-posting-date-face
     " Checking balance"            ledger-font-payee-cleared-face
     "  Assets:Checking  "          ledger-font-posting-account-face
     "            (1/4 * $123.00)"  ledger-font-posting-amount-face
     "  Equity:Opening Balances  "  ledger-font-posting-account-face
     "          = $500.00"          ledger-font-posting-amount-face)))


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
 '("2014-11-29"                             ledger-font-posting-date-face
   " colruyt"                               ledger-font-payee-uncleared-face
   "    k:eten:olie  "                      ledger-font-posting-account-face
   "                   (€11.52 - €2.00)"    ledger-font-posting-amount-face
   "    t:colruyt  "                        ledger-font-posting-account-face
   "                               €-9.52"  ledger-font-posting-amount-face
   "2014-12-03=2014-11-29"                  ledger-font-posting-date-face
   " colruyt"                               ledger-font-payee-uncleared-face
   "    t:colruyt  "                        ledger-font-posting-account-face
   "                               €35.90"  ledger-font-posting-amount-face
   "    b:bkbc:cmfrt  "                     ledger-font-posting-account-face
   "                           €-35.90"     ledger-font-posting-amount-face
   "2014-12-03=2014-11-29"                  ledger-font-posting-date-face
   " colruyt"                               ledger-font-payee-uncleared-face
   "    t:colruyt  "                        ledger-font-posting-account-face
   "                                €9.52"  ledger-font-posting-amount-face
   "    b:bkbc:cmfrt  "                     ledger-font-posting-account-face
   "                            €-9.52"     ledger-font-posting-amount-face
   "2014-12-01"                             ledger-font-posting-date-face
   " azg"                                   ledger-font-payee-uncleared-face
   "    k:varia-k:filantropie:azg  "        ledger-font-posting-account-face
   "                €7.44"                  ledger-font-posting-amount-face)))


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
 '("2014-11-26"                                 ledger-font-posting-date-face
   " 0bbde77a"                                  ledger-font-payee-uncleared-face
   "    ea91d4b4:e45cece9:55d77630:6fe97d2b  "  ledger-font-posting-account-face
   "      A4.13"                                ledger-font-posting-amount-face
   "    155ee966:be2467c7:e75bfd51:ce9edcc0  "  ledger-font-posting-account-face
   "     A-4.13"                                ledger-font-posting-amount-face
   "2014-11-27=2014-11-15"                      ledger-font-posting-date-face
   " e92211c9"                                  ledger-font-payee-cleared-face
   "    94f2b492:e361e00d:6e702d4d:13531a9d  "  ledger-font-posting-account-face
   "     A38.98"                                ledger-font-posting-amount-face
   "    e6ee9cc7:b91c4f8d:5746af7b:9e12005e"    ledger-font-posting-account-face)))


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
 '("2006/03/10"                   ledger-font-posting-date-face
   " Opening balance"             ledger-font-payee-uncleared-face
   "  Assets:X  "                 ledger-font-posting-account-face
   "£4000"                        ledger-font-posting-amount-face
   "  Equity:Opening Balance"     ledger-font-posting-account-face
   "P 2006/03/05 FOO £1.40
P 2006/03/06 BAR £1.50
"                                 ledger-font-price-directive-face
   "2006/05/15"                   ledger-font-posting-date-face
   " Interest"                    ledger-font-payee-uncleared-face
   "  Income:Taxable:Interest  "  ledger-font-posting-account-face
   "£-28.75"                      ledger-font-posting-amount-face
   "  Expenses:Tax:Income Tax  "  ledger-font-posting-account-face
   "£5.75"                        ledger-font-posting-amount-face
   "  Assets:X"                   ledger-font-posting-account-face)))


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
 '("account Assets:Bank1:Checking
"                                  ledger-font-account-directive-face ; FIXME try to fontify the full directive, note included?
   "account Assets:Bank5:LDD
"                                  ledger-font-account-directive-face)))


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
 '("2014/11/02"                                    ledger-font-posting-date-face
   " beer"                                         ledger-font-payee-cleared-face
   "    Expenses:Drinks  "                         ledger-font-posting-account-face
   "                              5 EUR"           ledger-font-posting-amount-face
   "    Assets:Checking  "                         ledger-font-posting-account-face
   "                             -5 EUR"           ledger-font-posting-amount-face
   "2014/11/02=2014/11/03"                         ledger-font-posting-date-face
   " salary"                                       ledger-font-payee-cleared-face
   "    Assets:Checking  "                         ledger-font-posting-account-face
   "                             10 EUR"           ledger-font-posting-amount-face
   "    Income  "                                  ledger-font-posting-account-face
   "                                     -10 EUR"  ledger-font-posting-amount-face
   "2014/11/02"                                    ledger-font-posting-date-face
   " burger"                                       ledger-font-payee-cleared-face
   "    Expenses:Food  "                           ledger-font-posting-account-face
   "                                7 EUR"         ledger-font-posting-amount-face
   "    Assets:Checking  "                         ledger-font-posting-account-face
   "                             -7 EUR"           ledger-font-posting-amount-face)))


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
 '("
; Budget ========================================================
" ledger-font-comment-face
"~ Weekly
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15" ledger-font-periodic-xact-face
    "~ Biweekly until 2014-04-03
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15" ledger-font-periodic-xact-face
    "~ Biweekly from 2014-04-03 until 2014-09-18
    b9f7b467:53dd031c:379c2da8               $100.00
    df0fcf15" ledger-font-periodic-xact-face
    "; Pricing =======================================================
" ledger-font-comment-face
"P 2014-01-01 WRCOM $100.00
P 2014-01-01 VFDAY $100.00
P 2014-01-01 VHDAY $100.00
P 2014-01-01 WBDES $100.00
" ledger-font-price-directive-face
"; 2014-11 =======================================================
" ledger-font-comment-face
"2014-11-01" ledger-font-posting-date-face
" 921edb9b" ledger-font-payee-cleared-face
"    ; 53dd031c" ledger-font-comment-face
"    b9f7b467:53dd031c:379c2da8  " ledger-font-posting-account-face
"            $-100.00" ledger-font-posting-amount-face
"    df0fcf15:59c361db:4972bdf0:f3e78683" ledger-font-posting-account-face
"2014-11-01" ledger-font-posting-date-face
" 921edb9b" ledger-font-payee-cleared-face
"    ; 53dd031c" ledger-font-comment-face
"    b9f7b467:53dd031c:379c2da8" ledger-font-posting-account-face
"    df0fcf15:59c361db:4972bdf0:f3e78683  " ledger-font-posting-account-face
"    $100.00" ledger-font-posting-amount-face
"2014-11-01=2014-11-02" ledger-font-posting-date-face
" 921edb9b" ledger-font-payee-cleared-face
"    ; 53dd031c" ledger-font-comment-face
"    b9f7b467:53dd031c:379c2da8  " ledger-font-posting-account-face
"            $-100.00" ledger-font-posting-amount-face
"    df0fcf15:59c361db:4972bdf0:f3e78683" ledger-font-posting-account-face)))


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
 '("2014/03/11"                                      ledger-font-posting-date-face
   " Whatever"                                       ledger-font-payee-uncleared-face
   "    A  "                                         ledger-font-posting-account-face
   "                                             0"  ledger-font-posting-amount-face
   "    B  "                                         ledger-font-posting-account-face
   "                                         6,75€"  ledger-font-posting-amount-face
   "    C  "                                         ledger-font-posting-account-face
   "                                        -6,75€"  ledger-font-posting-amount-face)))


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
 '("2014/08/23"                                  ledger-font-posting-date-face
   " demo fontification error"                   ledger-font-payee-uncleared-face
   "    Expense:AAA  "                           ledger-font-posting-account-face
   "                            € 10,00"         ledger-font-posting-amount-face
   "    Expense:BBB  "                           ledger-font-posting-account-face
   "                            € 20,00"         ledger-font-posting-amount-face
   "    cash"                                    ledger-font-posting-account-face
   "    Expense:CCC  "                           ledger-font-posting-account-face
   "                            € 10,00"         ledger-font-posting-amount-face
   "    card  "                                  ledger-font-posting-account-face
   "                                  -€ 10,00"  ledger-font-posting-amount-face)))


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
 '("account Assets:StarOne Savings
"                              ledger-font-account-directive-face
   "2014/05/07"                ledger-font-posting-date-face
   "   CoinStar Jar of Coins"  ledger-font-payee-uncleared-face
   "    cash  "                ledger-font-posting-account-face
   "   -$27.69"                ledger-font-posting-amount-face
   "    starone  "             ledger-font-posting-account-face
   " $27.69"                   ledger-font-posting-amount-face
   "    cash  "                ledger-font-posting-account-face
   "   -$7.96"                 ledger-font-posting-amount-face
   "    starone  "             ledger-font-posting-account-face
   " $7.96"                    ledger-font-posting-amount-face
   "    cash  "                ledger-font-posting-account-face
   "   -$156.89"               ledger-font-posting-amount-face
   "    starone  "             ledger-font-posting-account-face
   " $156.89"                  ledger-font-posting-amount-face
   "    cash  "                ledger-font-posting-account-face
   "    -$45.13"               ledger-font-posting-amount-face
   "    starone  "             ledger-font-posting-account-face
   "  $45.13"                  ledger-font-posting-amount-face)))


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
 '("2011/05/08"               ledger-font-posting-date-face
   " Some expense"            ledger-font-payee-cleared-face
   "  Wallets:Some Person  "  ledger-font-posting-account-face
   "$9.31"                    ledger-font-posting-amount-face
   "  Income:Some income"     ledger-font-posting-account-face
   "2011/05/17"               ledger-font-posting-date-face
   " Another expense"         ledger-font-payee-cleared-face
   "  Wallets:Some Person  "  ledger-font-posting-account-face
   "$10.38"                   ledger-font-posting-amount-face
   "  Income:Some income"     ledger-font-posting-account-face
   "2011/09/11"               ledger-font-posting-date-face
   " Another expense"         ledger-font-payee-uncleared-face
   "  Expenses:Expensive  "   ledger-font-posting-account-face
   "$585.00"                  ledger-font-posting-amount-face
   "  Wallets:Some Person"    ledger-font-posting-account-face)))


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
   '("2010/12/01"                  ledger-font-posting-date-face
     " Checking balance  "         ledger-font-payee-cleared-face
     "; note"                      ledger-font-comment-face
     "  Assets:Checking  "         ledger-font-posting-account-face
     "                 $1,000.00"  ledger-font-posting-amount-face
     "  Equity:Opening Balances"   ledger-font-posting-account-face
     "2010/12/01"                  ledger-font-posting-date-face
     " Checking balance  "         ledger-font-payee-uncleared-face
     "; note"                      ledger-font-comment-face
     "  Assets:Checking  "         ledger-font-posting-account-face
     "                 $1,000.00"  ledger-font-posting-amount-face
     "  Equity:Opening Balances"   ledger-font-posting-account-face)))


(ert-deftest ledger-fontify/test-016 ()
  "Bug 884 - ledger-font-highlight-face does not highlight first line of buffer"
  :tags '(font regress)

  (ledger-test-font-lock
   ;; no space is intentional between '"' and '2' at beginning of below line
   "2013/01/03 * (6106152) Dr About
    Dépense:Santé:Médecin                    23,00 €
    Actif:Courant:BnpCc
"
   '("2013/01/03"                   ledger-font-posting-date-face
     " (6106152)"                   ledger-font-code-face
     " Dr About"                    ledger-font-payee-cleared-face
     "    Dépense:Santé:Médecin  "  ledger-font-posting-account-face
     "                  23,00 €"    ledger-font-posting-amount-face
     "    Actif:Courant:BnpCc"      ledger-font-posting-account-face)))


;; https://groups.google.com/d/msg/ledger-cli/9zyWZW_fJmk/G56uVsqv0FAJ
;; FIXME
; Some others may have encountered the same issue I did, so I am posting
; this. With the new fontification scheme my cleared transactions did
; not have the posting accounts & amounts highlighted. It turns out you
; need to set ledger-fontify-xact-state-overrides to nil to get
; fontification of amounts & accounts with cleared transactions.


(provide 'fontify-test)

;;; fontify-test.el ends here
