;;; context-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
      (should (eq (ledger-context-current-field context) 'commoditized-amount))
      (should (equal "25,17 €"
                     (ledger-context-field-value context 'commoditized-amount))))))


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
      (should (eq (ledger-context-current-field context) 'commoditized-amount))
      (should (equal "25 €"
                     (ledger-context-field-value context 'commoditized-amount))))))


(ert-deftest ledger-context/test-003 ()
  "number in comments"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "    Dépense:Impôt:Local                      48,00 €  ; TH 2013"
   (should (equal (ledger-context-at-point)
                  '(acct-transaction indent ((indent "   " 1) (status nil nil) (account "Dépense:Impôt:Local" 5) (separator "                      " 24) (commoditized-amount "48,00 €" 46) (separator nil nil) (comment nil nil))))))) ; FIXME: in the result of ledger-context-at-point, at the end, purely speaking we should be able to detect the values of separator and comment. As it is not used in any other part of the code, this bug can be let as is for the moment


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


(ert-deftest ledger-context/test-005 ()
  "Regress test for Issue #1
https://github.com/ledger/ledger-mode/issues/1"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "2016/08/12 KFC
    a  $7
    b

2016/08/12 * KFC
    a  $7
    b

2016/08/12 ! KFC
    a  $7
    b

2016/08/12 (1234) KFC
    a  $7
    b

2016/08/12 * (1234) KFC
    a  $7
    b

2016/08/12 ! (1234) KFC
    a  $7
    b

2016/08/12 KFC  ; comment
    a  $7
    b

2016/08/12 * KFC  ; comment
    a  $7
    b

2016/08/12 ! KFC  ; comment
    a  $7
    b

2016/08/12 (1234) KFC  ; comment
    a  $7
    b

2016/08/12 * (1234) KFC  ; comment
    a  $7
    b

2016/08/12 ! (1234) KFC  ; comment
    a  $7
    b

2016/08/12 ! (1234) KFC      ; comment
    a  $7
    b

2016/08/12   !   (1234)   KFC   ;   comment
    a  $7
    b
"
   (goto-char (point-min))              ; beginning-of-buffer
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 1) (payee "KFC" 12)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 33) (status "*" 44) (payee "KFC" 46)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 67) (status "!" 78) (payee "KFC" 80)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 101) (code "(1234)" 112) (payee "KFC" 119)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 140) (status "*" 151) (code "(1234)" 153) (payee "KFC" 160)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 181) (status "!" 192) (code "(1234)" 194) (payee "KFC" 201)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 222) (payee "KFC" 233) (separator "  " 236) (comment "comment" 240)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 265) (status "*" 276) (payee "KFC" 278) (separator "  " 281) (comment "comment" 285)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 310) (status "!" 321) (payee "KFC" 323) (separator "  " 326) (comment "comment" 330)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 355) (code "(1234)" 366) (payee "KFC" 373) (separator "  " 376) (comment "comment" 380)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 405) (status "*" 416) (code "(1234)" 418) (payee "KFC" 425) (separator "  " 428) (comment "comment" 432)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 457) (status "!" 468) (code "(1234)" 470) (payee "KFC" 477) (separator "  " 480) (comment "comment" 484)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 509) (status "!" 520) (code "(1234)" 522) (payee "KFC" 529) (separator "      " 532) (comment "comment" 540)))))
   (ledger-navigate-next-xact-or-directive)
   (should (equal (ledger-context-at-point)
                  '(xact date ((date "2016/08/12" 565) (status "!" 578) (code "(1234)" 582) (payee "KFC" 591) (separator "   " 594) (comment "comment" 601)))))
   ))


(ert-deftest ledger-context/test-006 ()
  "Regress test for Issue #1
https://github.com/ledger/ledger-mode/issues/1"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "2016/08/12 KFC
 a  $7
 b

2016/08/12 KFC
	a		$7
	b
"
   (goto-char (point-min))              ; beginning-of-buffer
   (forward-line 1)
   (should (equal (ledger-context-at-point)
                  '(acct-transaction indent ((indent " " 16) (account "a" 17) (separator "  " 18) (commoditized-amount "$7" 20)))))
   (forward-line 1)
   (should (equal (ledger-context-at-point)
                  '(acct-transaction indent ((indent " " 23) (account "b" 24)))))
   (ledger-navigate-next-xact-or-directive)
   (forward-line 1)
   (should (equal (ledger-context-at-point)
                  '(acct-transaction indent ((indent "	" 42) (account "a" 43) (separator "		" 44) (commoditized-amount "$7" 46)))))
   (forward-line 1)
   (should (equal (ledger-context-at-point)
                  '(acct-transaction indent ((indent "	" 49) (account "b" 50)))))
   ))


(ert-deftest ledger-context/test-007 ()
  "Regress test for Bug 257
http://bugs.ledger-cli.org/show_bug.cgi?id=257"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "2004/05/01 * Checking balance
  Assets:Bank:Checking        1000.00
  Equity:Opening Balances
"
   (goto-char 63)                      ; on the amount 1000.00
   (let ((context (ledger-context-at-point)))
     (should (eq (ledger-context-current-field context) 'amount))
     (should (equal "1000.00"
                    (ledger-context-field-value context 'amount))))))


(ert-deftest ledger-context/test-008 ()
  "Regress test for Bug 257
http://bugs.ledger-cli.org/show_bug.cgi?id=257"
  :tags '(context regress)

  (ledger-tests-with-temp-file
   "2004/05/01 * Checking balance
  Assets:Bank:Checking        $1,000.00
  Equity:Opening Balances
"
   (goto-char 65)                      ; on the amount 1,000.00
   (let ((context (ledger-context-at-point)))
     (should (eq (ledger-context-current-field context) 'commoditized-amount))
     (should (equal "$1,000.00"
                    (ledger-context-field-value context 'commoditized-amount))))))


(provide 'context-test)

;;; context-test.el ends here
