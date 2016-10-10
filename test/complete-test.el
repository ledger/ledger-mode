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
