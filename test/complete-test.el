;;; complete-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-complete

;;; Code:
(require 'test-helper)


(ert-deftest ledger-complete/test-001 ()
  "Regress test for Bug 969+582
http://bugs.ledger-cli.org/show_bug.cgi?id=969
http://bugs.ledger-cli.org/show_bug.cgi?id=582"
  :tags '(complete regress)

  (let ((ledger-complete-in-steps t))
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
      (call-interactively #'completion-at-point)
      (should
       (equal (buffer-string)
              "2013/05/19 Retrait
    Dépense:Alimentation:Épicerie  35 €  ; Marché
    Dépense:
    Dépense:Alimentation:Épicerie  8,1 €  ; Arum café
    Dépense:Liquide
    * Passif:Crédit:BanqueAccord              -60,00 €")))))


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

(ert-deftest ledger-complete/test-complete-account-without-amount ()
  "https://github.com/ledger/ledger-mode/issues/141"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    blo"
    (goto-char (point-max))
    (call-interactively 'completion-at-point)
    (should
     (equal (buffer-string)
            "2010/04/08 payee
    blah                1 €
    bloop

2010/04/09 payee
    bloop"))))

(ert-deftest ledger-complete/test-complete-single-payee ()
  "https://github.com/ledger/ledger-mode/issues/181"
  :tags '(complete regress)
  (ledger-tests-with-temp-file
      "2019/06/28 Foobar
    Expenses:Baz                               11.99 CAD
    Assets:Cash

2019/06/20 Foo"
    (goto-char (point-max))
    (call-interactively 'completion-at-point)
    (should
     (equal (buffer-string)
            "2019/06/28 Foobar
    Expenses:Baz                               11.99 CAD
    Assets:Cash

2019/06/20 Foobar"))))

(ert-deftest ledger-complete/test-find-accounts-in-buffer ()
  (let ((ledger "*** Expenses
account Expenses:Accomodation
account Assets:Cash  ; some comment
account Assets:Current
;    alias 1187465S022    -- Ideally this line could be uncommented
commodity EUR
;    format 1,000.00 EUR  -- Ideally this line could be uncommented
tag ofxid
2018/05/07 * Company
    Assets:Current  -38.33 EUR
    ; ofxid: someid
    Expenses:Utilities:Insurance  38.00 EUR
    [Dimensions:Foo]  30.00 EUR
    [Expenses:Accomodation]  8.33 EUR
    [Dimensions:Equity]  -38.33 EUR
    (Something)  43.00 EUR
"))
    (with-temp-buffer
      (insert ledger)
      (should (equal
               (ledger-accounts-list-in-buffer)
               (list
                "Assets:Cash"
                "Assets:Current"
                "Dimensions:Equity"
                "Dimensions:Foo"
                "Expenses:Accomodation"
                "Expenses:Utilities:Insurance"
                "Something"))))))

(ert-deftest ledger-complete/test-find-accounts-with-spaces-in-buffer ()
  (let ((ledger "*** Expenses
account Expenses:The Bakery
"))
    (with-temp-buffer
      (insert ledger)
      (should (equal
               (ledger-accounts-list-in-buffer)
               (list
                "Expenses:The Bakery"))))))

(ert-deftest ledger-complete/test-ledger-accounts-exclude-function ()
  ;; TODO: Why doesn't this work in batch?
  :tags '(interactive)
  (with-temp-buffer
    (insert "account Assets:Checking:Bank A
    assert date<=[1990-01-01]
account Assets:Checking:Bank B")
    (let ((ledger-accounts-exclude-function
           (lambda (i) "Exclude all entries with a subdirective."
             (cdr i))))
      (should (equal (ledger-accounts-list-in-buffer)
                     (list "Assets:Checking:Bank B"))))
    (let ((ledger-accounts-exclude-function (lambda (_) t)))
      (should (equal (ledger-accounts-list-in-buffer)
                     nil)))
    (let ((ledger-accounts-exclude-function (lambda (_) nil)))
      (should (equal (ledger-accounts-list-in-buffer)
                     (list "Assets:Checking:Bank A"
                           "Assets:Checking:Bank B"))))))

(provide 'complete-test)

;;; complete-test.el ends here
