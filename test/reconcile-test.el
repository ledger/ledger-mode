;;; reconcile-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-reconcile

;;; Code:
(require 'test-helper)


(ert-deftest ledger-reconcile/test-001 ()
  "Regress test for Bug 1107
http://bugs.ledger-cli.org/show_bug.cgi?id=1107"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; this moves to *reconcile* buffer
    (other-window 1)                ; go to *ledger* buffer
    (insert " ")                    ; simulate modification of ledger buffer
    (delete-char -1)
    (other-window 1)                ; back to *reconcile* buffer
    (ledger-reconcile-save)         ; key 's'
    (should ;; current buffer should be *reconcile* buffer
     (equal (buffer-name)           ; current buffer name
            ledger-reconcile-buffer-name))
    (other-window 1)                ; switch to *other* window
    (should ;; Expected: this must be ledger buffer
     (equal (buffer-name)           ; current buffer name
            (buffer-name ledger-buffer)))))


(ert-deftest ledger-reconcile/test-002 ()
  "Regress test for Bug 1060 + Bug 1039
http://bugs.ledger-cli.org/show_bug.cgi?id=1060
http://bugs.ledger-cli.org/show_bug.cgi?id=1039"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (forward-line 2)                    ; because of ledger-reconcile-buffer-header
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-finish)                     ; C-c C-c
    (should ;; Expected: buffer reconcile must still exist and be selected
     (equal ledger-reconcile-buffer-name
            (buffer-name (window-buffer (selected-window)))))))


(ert-deftest ledger-reconcile/test-003 ()
  "Regress test for Bug 1060
http://bugs.ledger-cli.org/show_bug.cgi?id=1060

If `ledger-reconcile-finish-force-quit' is set, reconcile window is killed"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (setq ledger-reconcile-finish-force-quit t)
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (forward-line 2)                    ; because of ledger-reconcile-buffer-header
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-finish)                     ; C-c C-c
    (should ;; Expected: reconcile buffer has been killed
     (equal nil (get-buffer-window ledger-reconcile-buffer-name)))))


(ert-deftest ledger-reconcile/test-004 ()
  "Regress test for Bug 1108 and Bug 1061
http://bugs.ledger-cli.org/show_bug.cgi?id=1108
http://bugs.ledger-cli.org/show_bug.cgi?id=1061"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
"2014/11/10  EDF
    Dépense:Maison:Service:Électricité    36,23 €
    Actif:Courant:BnpCc

2014/11/14  Banque Accord Retrait
    Passif:Crédit:BanqueAccord              60,00 €
    Actif:Courant:BnpCc
"
    (goto-char 150)                 ; line 6, before A of BanqueAccord
    (let ((context (ledger-context-at-point)))
      (should (eq (ledger-context-current-field context) 'account))
      (should (equal "Passif:Crédit:BanqueAccord"
                     (ledger-context-field-value context 'account))))))


(ert-deftest ledger-reconcile/test-005 ()
  "Regress test for Bug 1105+875
http://bugs.ledger-cli.org/show_bug.cgi?id=1105
http://bugs.ledger-cli.org/show_bug.cgi?id=875"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
"2008/10/16 (2090) Bountiful Blessings Farm Williamsport
    Expenses:Food:Groceries                  $ 37.50  ; [=2008/10/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2008/11/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2008/12/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2009/01/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2009/02/01]
    Expenses:Food:Groceries                  $ 37.50  ; [=2009/03/01]
    Assets:Checking
"

    (setq ledger-reconcile-buffer-header ""
          ledger-reconcile-buffer-line-format "%(date)s %-30(payee)s %-22(account)s %10(amount)s\n"
          ledger-reconcile-buffer-payee-max-chars 30
          ledger-reconcile-buffer-account-max-chars 22)
    (ledger-reconcile "Expenses:Food:Groceries" '(0 "$"))
    (switch-to-buffer ledger-reconcile-buffer-name)
    (should (equal
             "2008/10/16 Bountiful Blessings Farm Will… …penses:Food:Groceries    $ 37.50
2008/10/16 Bountiful Blessings Farm Will… …penses:Food:Groceries    $ 37.50
2008/10/16 Bountiful Blessings Farm Will… …penses:Food:Groceries    $ 37.50
2008/10/16 Bountiful Blessings Farm Will… …penses:Food:Groceries    $ 37.50
2008/10/16 Bountiful Blessings Farm Will… …penses:Food:Groceries    $ 37.50
2008/10/16 Bountiful Blessings Farm Will… …penses:Food:Groceries    $ 37.50"
             (buffer-string)))))


(ert-deftest ledger-reconcile/test-006 ()
  "Regress test for Bug 1104
http://bugs.ledger-cli.org/show_bug.cgi?id=1104"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (setq ledger-reconcile-buffer-header "")
    (ledger-reconcile "Expenses:Books" '(0 "$"))
    (should-not
     (equal nil (get-buffer-window ledger-reconcile-buffer-name)))))


(ert-deftest ledger-reconcile/test-007 ()
  "Regress test for Bug 1059
http://bugs.ledger-cli.org/show_bug.cgi?id=1059"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
"2014/03/03 * Retrait
    Dépense:Alimentation:Épicerie            20,00 €
    Dépense:Alimentation:Restaurant          23,80 €
    Dépense:Alimentation:Restaurant          11,50 €
    Actif:Remboursement:Cie  1,50 €
    Dépense:Liquide
    Passif:Crédit:BanqueAccord              -60,00 €
"
    (goto-char 206)
    (ledger-toggle-current)             ; C-c C-c
    (should
     (equal
      "2014/03/03 Retrait
    * Dépense:Alimentation:Épicerie          20,00 €
    * Dépense:Alimentation:Restaurant        23,80 €
    * Dépense:Alimentation:Restaurant        11,50 €
    Actif:Remboursement:Cie    1,50 €
    * Dépense:Liquide
    * Passif:Crédit:BanqueAccord            -60,00 €
"
             (buffer-string)))))


(ert-deftest ledger-reconcile/test-008 ()
  "Regress test for Bug 1056
http://bugs.ledger-cli.org/show_bug.cgi?id=1056"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
"2014/04/03 www.amazon.fr
    Dépense:Loisir:Ordi:Matériel            101,50 €  ; disque dur portable 2,5\" 2000 Go
    Dépense:Maison:Service:Poste
    * Passif:Crédit:BanqueAccord           -171,63 €
"
    (setq ledger-reconcile-default-commodity "€") ; FIXME This must be set even if below call (ledger-reconcile "Dépense" '(0 "€")) is using "€". Is this a bug?
    (ledger-reconcile "Dépense" '(0 "€"))
    (should-not ;; ledger reconcile must exists and no error prevented to go to this point
     (equal nil (get-buffer-window ledger-reconcile-buffer-name)))))


(ert-deftest ledger-reconcile/test-009 ()
  "Regress test for Bug 1040+915
http://bugs.ledger-cli.org/show_bug.cgi?id=1040
http://bugs.ledger-cli.org/show_bug.cgi?id=915"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (forward-line 6)
    (ledger-reconcile-toggle)
    (ledger-reconcile-toggle)
    (let ((line-before-save (line-number-at-pos)))
      (ledger-reconcile-save)             ; key 's'
      (should ;; Expected: line position is kept
       (eq line-before-save (line-number-at-pos)))
      (should ;; current buffer should be *reconcile* buffer
       (equal (buffer-name)           ; current buffer name
              ledger-reconcile-buffer-name)))))


(ert-deftest ledger-reconcile/test-010 ()
  "Regress test for Bug 986
http://bugs.ledger-cli.org/show_bug.cgi?id=986"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger

    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (should
     (equal (buffer-string)     ; default sort is by ledger file order
      "Reconciling account Assets:Checking

2011/01/14      Bank                                               Assets:Checking                      $ -300.00
2011/01/19      Grocery Store                                      Assets:Checking                       $ -44.00
2011/01/25      Bank                                               Assets:Checking                     $ 5,500.00
2011/01/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/27      Bookstore                                          Assets:Checking                       $ -20.00
2011/12/01      Sale                                               Assets:Checking                        $ 30.00" ) )
    (ledger-reconcile-quit)

    (setq ledger-reconcile-sort-key "(date)") ; sort by date
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (should
     (equal (buffer-string)
      "Reconciling account Assets:Checking

2011/01/14      Bank                                               Assets:Checking                      $ -300.00
2011/01/19      Grocery Store                                      Assets:Checking                       $ -44.00
2011/01/25      Bank                                               Assets:Checking                     $ 5,500.00
2011/01/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/27      Bookstore                                          Assets:Checking                       $ -20.00
2011/12/01      Sale                                               Assets:Checking                        $ 30.00"))
    (ledger-reconcile-quit)

    (setq ledger-reconcile-sort-key "(amount)") ; sort by amount
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (should
     (equal (buffer-string)             ; sort by ledger file order
      "Reconciling account Assets:Checking

2011/01/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/01/14      Bank                                               Assets:Checking                      $ -300.00
2011/01/19      Grocery Store                                      Assets:Checking                       $ -44.00
2011/04/27      Bookstore                                          Assets:Checking                       $ -20.00
2011/12/01      Sale                                               Assets:Checking                        $ 30.00
2011/01/25      Bank                                               Assets:Checking                     $ 5,500.00"))
    (ledger-reconcile-quit)

    (setq ledger-reconcile-sort-key "(payee)") ; sort by payee
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (should
     (equal (buffer-string)
      "Reconciling account Assets:Checking

2011/01/14      Bank                                               Assets:Checking                      $ -300.00
2011/01/25      Bank                                               Assets:Checking                     $ 5,500.00
2011/04/27      Bookstore                                          Assets:Checking                       $ -20.00
2011/01/19      Grocery Store                                      Assets:Checking                       $ -44.00
2011/12/01      Sale                                               Assets:Checking                        $ 30.00
2011/01/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00"))
    (ledger-reconcile-quit)

    (setq ledger-reconcile-sort-key "(0)") ; sort by ledger file order
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (should
     (equal (buffer-string)
      "Reconciling account Assets:Checking

2011/01/14      Bank                                               Assets:Checking                      $ -300.00
2011/01/19      Grocery Store                                      Assets:Checking                       $ -44.00
2011/01/25      Bank                                               Assets:Checking                     $ 5,500.00
2011/01/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/25      Tom's Used Cars                                    Assets:Checking                    $ -5,500.00
2011/04/27      Bookstore                                          Assets:Checking                       $ -20.00
2011/12/01      Sale                                               Assets:Checking                        $ 30.00"))
    (ledger-reconcile-quit)))


(ert-deftest ledger-reconcile/test-011 ()
  "Regress test for Bug 967
http://bugs.ledger-cli.org/show_bug.cgi?id=967"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (other-window 1)                ; go to *ledger* buffer
    (remove-hook 'kill-buffer-hook 'ledger-reconcile-quit t) ; needed for delete-other-windows
    (delete-other-windows)          ; C-x 1
    (set-frame-width (selected-frame) 100) ; needed for split-window-right
    (split-window-right)            ; C-x 3
    (switch-to-buffer-other-window ledger-reconcile-buffer-name) ; C-x 4 b

    (forward-line 2)
    (ledger-reconcile-toggle)

    (let ((right-window-before-save (window-in-direction 'right))
          (left-window-before-save (window-in-direction 'left)))
      (ledger-reconcile-save)             ; key 's'
      (should ;; Expected: window config is unchanged
       (eq right-window-before-save (window-in-direction 'right)))
      (should ;; Expected: window config is unchanged
       (eq left-window-before-save (window-in-direction 'left))))))


(ert-deftest ledger-reconcile/test-012 ()
  "Regress test for Bug 957
http://bugs.ledger-cli.org/show_bug.cgi?id=957"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      "2013/04/20 Petit Casino
    Dépense:Alimentation:Alcool               6,49 €
    Dépense:Alimentation:Épicerie
    Passif:Crédit:BanqueAccord              -14,94 €

2013/04/20 Les Tilleuls
    Dépense:Alimentation:Restaurant          18,40 €
    Passif:Crédit:BanqueAccord
"
    (setq ledger-reconcile-default-commodity "€")
    (ledger-reconcile "BanqueAccord" '(0 "€"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (forward-line 2)
    (ledger-reconcile-visit)
    (forward-line -1)
    (goto-char (line-beginning-position)) ; beginning-of-line
    (insert "    Dépense:Alimentation:Alcool    1,00 €
    Dépense:Alimentation:Alcool    1,00 €
    Dépense:Alimentation:Alcool    1,00 €
")
    (save-buffer)
    (switch-to-buffer-other-window ledger-reconcile-buffer-name)
    (ledger-reconcile-toggle)
    (switch-to-buffer-other-window ledger-buffer)
    (should
     (equal (buffer-string)
            "2013/04/20 Petit Casino
    Dépense:Alimentation:Alcool               6,49 €
    Dépense:Alimentation:Alcool    1,00 €
    Dépense:Alimentation:Alcool    1,00 €
    Dépense:Alimentation:Alcool    1,00 €
    Dépense:Alimentation:Épicerie
    ! Passif:Crédit:BanqueAccord            -14,94 €

2013/04/20 Les Tilleuls
    Dépense:Alimentation:Restaurant          18,40 €
    Passif:Crédit:BanqueAccord
"))
    (goto-char (point-min))  ; beginning-of-buffer
    (forward-line 1)
    (kill-line 4)
    (save-buffer)
    (switch-to-buffer-other-window ledger-reconcile-buffer-name)
    (forward-line -1)
    (ledger-reconcile-toggle)
    (switch-to-buffer-other-window ledger-buffer)
    (should
     (equal (buffer-string)
            "2013/04/20 Petit Casino
    Dépense:Alimentation:Épicerie
    Passif:Crédit:BanqueAccord              -14,94 €

2013/04/20 Les Tilleuls
    Dépense:Alimentation:Restaurant          18,40 €
    Passif:Crédit:BanqueAccord
"))))


(ert-deftest ledger-reconcile/test-013 ()
  "Regress test for Bug 906
http://bugs.ledger-cli.org/show_bug.cgi?id=906"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (goto-char 1040)
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (switch-to-buffer-other-window ledger-buffer)
    (should (= 1040 (point)))

    (goto-char 1265)
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (switch-to-buffer-other-window ledger-buffer)
    (should (= 1265 (point)))))


(ert-deftest ledger-reconcile/test-014 ()
  "Regress test for Bug 900
http://bugs.ledger-cli.org/show_bug.cgi?id=900"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (switch-to-buffer-other-window ledger-buffer)
    (save-buffer)
    (let ((ledger-buffer-name (buffer-name ledger-buffer)))
      (find-alternate-file temp-file)
      (ledger-reconcile "Expenses:Books" '(0 "$"))
      (should ;; Expected: buffer with same name
       (equal (buffer-name (current-buffer))
              ledger-buffer-name)))))


(ert-deftest ledger-reconcile/test-015 ()
  "Regress test for Bug 245
http://bugs.ledger-cli.org/show_bug.cgi?id=245"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file          ; Faces in *Reconcile* buffer
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (forward-line 2)       ; because of ledger-reconcile-buffer-header
    (should (eq 'ledger-font-reconciler-uncleared-face
                (get-text-property (point) 'font-lock-face)))
    (ledger-reconcile-toggle)           ; mark pending
    (forward-line -1)                   ; go back on pending line
    (should (eq 'ledger-font-reconciler-pending-face
                (get-text-property (point) 'font-lock-face)))))


(ert-deftest ledger-reconcile/test-016 ()
  "Keep line in *Reconcile* buffer"
  :tags '(reconcile baseline)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (forward-line 2)       ; because of ledger-reconcile-buffer-header
    (ledger-reconcile-toggle)           ; mark pending
    (forward-line 1)
    (let ((line-before-finish (line-number-at-pos)))
      (ledger-reconcile-finish)                     ; C-c C-c
      (should ;; Expected: line position is kept
       (eq line-before-finish (line-number-at-pos))))))


(ert-deftest ledger-reconcile/test-017 ()
  "Regress test for Bug 895
http://bugs.ledger-cli.org/show_bug.cgi?id=895"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Food" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (should
     (equal (buffer-string)
      "Reconciling account Food

2011/01/02      Grocery Store                                      Expenses:Food:Groceries                $ 65.00
2011/01/19      Grocery Store                                      Expenses:Food:Groceries                $ 44.00" ))))


(ert-deftest ledger-reconcile/test-018 ()
  "Regress test for Bug 886
http://bugs.ledger-cli.org/show_bug.cgi?id=886"
  :tags '(reconcile baseline)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (goto-char (point-max))  ; end-of-buffer
    (let ((line-before-toggle (line-number-at-pos)))
      (ledger-reconcile-toggle)           ; mark pending
      (should (eq line-before-toggle (line-number-at-pos)))
      (should (eq 'ledger-font-reconciler-pending-face
                (get-text-property (point) 'font-lock-face)))
      (ledger-reconcile-toggle)           ; mark pending
      (should (eq line-before-toggle (line-number-at-pos)))
      (should (eq 'ledger-font-reconciler-uncleared-face
                (get-text-property (point) 'font-lock-face))))))


(ert-deftest ledger-reconcile/test-019 ()
  "Regress test for Bug 879
http://bugs.ledger-cli.org/show_bug.cgi?id=879"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-reconcile-buffer-name)) ; IRL user select reconcile window
    (switch-to-buffer-other-window ledger-buffer)
    (ledger-reconcile "Food" '(0 "$")) ; launch a *second* time on *another* account
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (should ;; current buffer should be *reconcile* buffer
     (equal (buffer-name)           ; current buffer name
            ledger-reconcile-buffer-name))
    (other-window 1)                ; switch to *other* window
    (should ;; Expected: this must be ledger buffer
     (equal (buffer-name)           ; current buffer name
            (buffer-name ledger-buffer)))))


(ert-deftest ledger-reconcile/test-020 ()
  "Regress test for Bug 527
http://bugs.ledger-cli.org/show_bug.cgi?id=527"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   "2012-03-10 (#100) KFC
    Expenses:Food                $20.00
    Assets:Checking
"
   (ledger-reconcile "Expenses:Food" '(0 "$"))
   (switch-to-buffer ledger-reconcile-buffer-name)
   (should (equal
            "Reconciling account Expenses:Food

2012/03/10 #100 KFC                                                Expenses:Food                           $20.00"
            (buffer-string)))))


(ert-deftest ledger-reconcile/test-021 ()
  "Regress test for Bug 922
http://bugs.ledger-cli.org/show_bug.cgi?id=922"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   "2012/01/02 (03DIZ3Q) Bilip
    Nyu:sto                                      -12  B
    Foo:bar

2012/01/02 (03DIZ3Q) Bilip
    * Nyu:sto                                    -12  B
    Foo:bar
"
   (setq ledger-reconcile-buffer-header "")
   (ledger-reconcile "Nyu" '(0 "B"))
   (switch-to-buffer ledger-reconcile-buffer-name)
   (should (equal (buffer-string)
            "2012/01/02 03DIZ3Q Bilip                                              Nyu:sto                                  -12 B"))))


(ert-deftest ledger-reconcile/test-022 ()
  "Regress test for Bug 951
http://bugs.ledger-cli.org/show_bug.cgi?id=951"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   "2012-03-10 (#100) KFC
    Expenses:Food                $3,877.78
    Assets:Checking
"
   (ledger-reconcile "Expenses:Food" '(0 "$"))
   (switch-to-buffer ledger-reconcile-buffer-name)
   (should (equal (buffer-string)
            "Reconciling account Expenses:Food

2012/03/10 #100 KFC                                                Expenses:Food                        $3,877.78"))))


(ert-deftest ledger-reconcile/test-023 ()
  "Regress test for Bug 897
http://bugs.ledger-cli.org/show_bug.cgi?id=897"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   "2012-03-10 (#100) KFC  ; comment
    Expenses:Food                $3,877.78  ; comment
    * Assets:Checking                         ; comment
"
   (forward-line 1)                     ; go to posting not cleared
   (ledger-toggle-current)              ; C-c C-c
   (should (equal (buffer-string)
            "2012-03-10 * (#100) KFC  ; comment
    Expenses:Food                $3,877.78  ; comment
    Assets:Checking                           ; comment
"))))


(ert-deftest ledger-reconcile/test-024 ()
  "Regress test for Bug 396
http://bugs.ledger-cli.org/show_bug.cgi?id=396"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   ;; FIXME "KFC" surrounded with double quotes should work too
   "2012-03-10 (#100) Kentucky Fried Chicken aka 'KFC'
    Expenses:Food                $3,877.78
    Assets:Checking
"
   (ledger-reconcile "Expenses:Food" '(0 "$"))
   (switch-to-buffer ledger-reconcile-buffer-name)
   (should (equal (buffer-string)
                  "Reconciling account Expenses:Food

2012/03/10 #100 Kentucky Fried Chicken aka 'KFC'                   Expenses:Food                        $3,877.78"))))


(ert-deftest ledger-reconcile/test-025 ()
  "Regress test for Bug 262
http://bugs.ledger-cli.org/show_bug.cgi?id=262"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   "2011/11/16 Amazon.com
    Expenses:Entertainment:Misc               $32.64
    Assets:VWCU:Joint Checking
"
   (forward-line 2)
   (ledger-toggle-current-transaction)  ; C-c C-e
   (should
    (equal
     (buffer-string)
     "2011/11/16 * Amazon.com
    Expenses:Entertainment:Misc               $32.64
    Assets:VWCU:Joint Checking
"))
   (goto-char (point-min))  ; beginning-of-buffer
   (forward-line 1)
   (ledger-toggle-current)             ; C-c C-c
   (should
    (equal
     (buffer-string)
     "2011/11/16 Amazon.com
    Expenses:Entertainment:Misc               $32.64
    * Assets:VWCU:Joint Checking
"))
   (ledger-toggle-current)             ; C-c C-c
   (should
    (equal
     (buffer-string)
     "2011/11/16 * Amazon.com
    Expenses:Entertainment:Misc               $32.64
    Assets:VWCU:Joint Checking
"))))


(ert-deftest ledger-reconcile/test-026 ()
  "Regress test for Bug 262
http://bugs.ledger-cli.org/show_bug.cgi?id=262"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   "2011/11/16 Amazon.com
    Expenses:Entertainment:Misc               $32.64
    Assets:VWCU:Joint Checking
"
   (forward-line 1)
   (ledger-toggle-current)             ; C-c C-c
   (should
    (equal
     (buffer-string)
     "2011/11/16 Amazon.com
    * Expenses:Entertainment:Misc             $32.64
    Assets:VWCU:Joint Checking
"))))


(ert-deftest ledger-reconcile/test-027 ()
  "Keep position in ledger buffer after delete in *Reconcile* buffer"
  :tags '(reconcile baseline)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Expenses" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (forward-line 2)       ; because of ledger-reconcile-buffer-header
    (forward-line 4)       ; move to not be on first line of reconcile
    (let ((line-before-delete (line-number-at-pos)))
      (ledger-reconcile-delete)             ; key 'd'
      (should ;; Expected: line position is kept
       (eq line-before-delete (line-number-at-pos)))
      (should ;; current buffer should be *reconcile* buffer
       (equal (buffer-name)           ; current buffer name
              ledger-reconcile-buffer-name))
      (other-window 1)                ; switch to *other* window
      (should ;; Expected: this must be ledger buffer
       (equal (buffer-name)           ; current buffer name
              (buffer-name ledger-buffer)))
      (should (= 1322 (point))))))    ; expected on "Book Store" xact


(ert-deftest ledger-reconcile/test-028 ()
  "Keep position in ledger buffer after delete in *Reconcile* buffer"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Expenses" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (goto-char (point-max))             ; end-of-buffer
    (goto-char (line-beginning-position)) ; beginning-of-line
    (let ((line-before-delete (line-number-at-pos)))
      (ledger-reconcile-delete)             ; key 'd'
      (should ;; Expected: previous line
       (eq (1- line-before-delete) (line-number-at-pos))))))


(ert-deftest ledger-reconcile/test-029 ()
  "Commodity values starting with 0 are valid"
  :tags '(reconcile regress)

  (should (equal '(0.53 "$") (ledger-split-commodity-string "$0.53")))
  (should (equal '(0.53 "USD") (ledger-split-commodity-string "0.53 USD")))

  (should (equal '(-0.53 "$") (ledger-split-commodity-string "$-0.53")))
  (should (equal '(-0.53 "USD") (ledger-split-commodity-string "-0.53 USD"))))


(ert-deftest ledger-reconcile/test-030 ()
  "`ledger-narrow-on-reconcile' basic functionality

Regression test for #383.
https://github.com/ledger/ledger-mode/issues/383"
  :tags '(reconcile regress)

  (let ((ledger-narrow-on-reconcile t))
    (ledger-tests-with-temp-file
        demo-ledger
      (ledger-reconcile "Expenses:Books" '(0 "$"))
      (switch-to-buffer ledger-reconcile-buffer-name)
      (with-current-buffer ledger-reconcile-ledger-buf
        (should (equal (ledger-test-visible-buffer-string) "
2011/01/27 Book Store
  Expenses:Books                       $20.00
  Liabilities:MasterCard

2011/04/27 Bookstore
  Expenses:Books                       $20.00
  Assets:Checking
")))

      (setq ledger-post-account-alignment-column 2)
      (setq ledger-post-amount-alignment-column 45)
      (setq ledger-default-date-format "%Y/%m/%d")
      ;; buffer overlays should be refreshed after adding xact
      (ledger-reconcile-add "2011/06/15" "Bookstore")
      (with-current-buffer ledger-reconcile-ledger-buf
        (should (equal (ledger-test-visible-buffer-string) "
2011/01/27 Book Store
  Expenses:Books                       $20.00
  Liabilities:MasterCard

2011/04/27 Bookstore
  Expenses:Books                       $20.00
  Assets:Checking

2011/06/15 Bookstore
  Expenses:Books                      $ 20.00
  Assets:Checking
"))))))


(ert-deftest ledger-reconcile/test-031 ()
  "Regression test for #416
https://github.com/ledger/ledger-mode/issues/416"

  (ledger-tests-with-temp-file
      "\
2011/01/27 * Book Store
  Expenses:Books                       $20.00
  Liabilities:MasterCard

2011/02/15 * Bank
  Liabilities:MasterCard               $20.00
  Assets:Checking

2011/02/27 Book Store
  Expenses:Books                       $20.00
  Liabilities:MasterCard
"

    (ledger-reconcile "Liabilities:MasterCard" (ledger-split-commodity-string "$20"))))


(ert-deftest ledger-reconcile/test-032 ()
  "Regression test for #408.

https://github.com/ledger/ledger-mode/issues/408"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
   demo-ledger
   (let ((ledger-reconcile-default-date-format "%Y-%m-%d"))
     (ledger-reconcile "Expenses:Books" '(0 "$")))
   (set-buffer ledger-reconcile-buffer-name)
   (should
    (equal
     (buffer-string)
     "Reconciling account Expenses:Books

2011-01-27      Book Store                                         Expenses:Books                         $ 20.00
2011-04-27      Bookstore                                          Expenses:Books                         $ 20.00"))))


(ert-deftest ledger-reconcile/test-s-helpers ()
  "`ledger-reconcile-s-left' and `-s-right' are no-ops when string is short enough."
  :tags '(reconcile)
  (should (equal "abc" (ledger-reconcile-s-left 5 "abc")))
  (should (equal "abc" (ledger-reconcile-s-right 5 "abc")))
  (should (equal "abc" (ledger-reconcile-s-left 3 "abc")))
  (should (equal "abc" (ledger-reconcile-s-right 3 "abc")))
  ;; Truncation forms
  (should (equal "abc" (ledger-reconcile-s-left 3 "abcdef")))
  (should (equal "def" (ledger-reconcile-s-right 3 "abcdef"))))


(ert-deftest ledger-reconcile/test-is-stdin ()
  "`ledger-is-stdin' recognises stdin variations."
  :tags '(reconcile)
  (should (ledger-is-stdin ""))
  (should (ledger-is-stdin "<stdin>"))
  (should (ledger-is-stdin "/dev/stdin"))
  (should-not (ledger-is-stdin "/tmp/foo.dat")))


(ert-deftest ledger-reconcile/test-get-buffer-error ()
  "`ledger-reconcile-get-buffer' errors when no buffer is set."
  :tags '(reconcile)
  (should-error (ledger-reconcile-get-buffer '(nil . 0))))


(ert-deftest ledger-reconcile/test-insert-effective-date-function ()
  "Function form of `ledger-reconcile-insert-effective-date' is called."
  :tags '(reconcile)
  (let* ((called nil)
         (ledger-reconcile-insert-effective-date (lambda () (setq called t) nil)))
    (cl-letf (((symbol-function 'ledger-insert-effective-date)
               (lambda (&rest _) nil)))
      (ledger-reconcile-insert-effective-date)
      (should called))))


(ert-deftest ledger-reconcile/test-insert-effective-date-non-function ()
  "Non-function truthy value of `ledger-reconcile-insert-effective-date'
calls the inserter."
  :tags '(reconcile)
  (let ((called nil)
        (ledger-reconcile-insert-effective-date t))
    (cl-letf (((symbol-function 'ledger-insert-effective-date)
               (lambda (&rest _) (setq called t))))
      (ledger-reconcile-insert-effective-date)
      (should called))))


(ert-deftest ledger-reconcile/test-format-posting-cleared-face ()
  "`ledger-reconcile-format-posting' sets cleared face for cleared postings."
  :tags '(reconcile)
  (with-temp-buffer
    (let ((fmt (ledger-reconcile-compile-format-string "%(date)s %(account)s %(amount)s\n"))
          (where (cons (current-buffer) 1))
          (beg (point)))
      (ledger-reconcile-format-posting beg where fmt
                                       "2024/01/01" "" 'cleared
                                       "Foo" "Acct" "$1")
      (goto-char beg)
      (should (eq 'ledger-font-reconciler-cleared-face
                  (get-text-property (point) 'font-lock-face))))))


(ert-deftest ledger-reconcile/test-toggle-cleared-face ()
  "Toggling with `ledger-reconcile-toggle-to-pending' nil yields cleared face."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (let ((ledger-reconcile-toggle-to-pending nil))
      (ledger-reconcile "Assets:Checking" '(0 "$"))
      (select-window (get-buffer-window ledger-reconcile-buffer-name))
      (forward-line 2)
      (ledger-reconcile-toggle)
      (forward-line -1)
      (should (eq 'ledger-font-reconciler-cleared-face
                  (get-text-property (point) 'font-lock-face))))))


(ert-deftest ledger-reconcile/test-add ()
  "`ledger-reconcile-add' inserts a transaction and refreshes."
  :tags '(reconcile)
  (let ((ledger-narrow-on-reconcile nil))
    (ledger-tests-with-temp-file
        demo-ledger
      (ledger-reconcile "Expenses:Books" '(0 "$"))
      (select-window (get-buffer-window ledger-reconcile-buffer-name))
      (cl-letf (((symbol-function 'ledger-read-date)
                 (lambda (&rest _) "2024/01/01")))
        (ledger-reconcile-add "2024/01/01" "Bookstore  $5.00"))
      (with-current-buffer ledger-reconcile-ledger-buf
        (should (string-match-p "2024/01/01 Bookstore" (buffer-string)))))))


(ert-deftest ledger-reconcile/test-add-interactive ()
  "`ledger-reconcile-add' prompts for date and transaction interactively."
  :tags '(reconcile)
  (let ((ledger-narrow-on-reconcile nil))
    (ledger-tests-with-temp-file
        demo-ledger
      (ledger-reconcile "Expenses:Books" '(0 "$"))
      (select-window (get-buffer-window ledger-reconcile-buffer-name))
      (cl-letf (((symbol-function 'ledger-read-date)
                 (lambda (&rest _) "2024/01/01"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "Bookstore  $5.00")))
        (call-interactively #'ledger-reconcile-add))
      (with-current-buffer ledger-reconcile-ledger-buf
        (should (string-match-p "2024/01/01 Bookstore" (buffer-string)))))))


(ert-deftest ledger-reconcile/test-marker-where-xact-is ()
  "`ledger-marker-where-xact-is' returns line-no of posting when not whole-tx."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (setq ledger-reconcile-ledger-buf (current-buffer))
    (let ((ledger-clear-whole-transactions nil))
      (let* ((emacs-xact (list "<stdin>" 5 nil nil "Foo"))
             (posting (list 7 "Acct" "$1" nil))
             (where (ledger-marker-where-xact-is emacs-xact posting)))
        (should (bufferp (car where)))
        (should (= 7 (cdr where))))
      ;; -1 posting falls through to xact line.
      (let* ((emacs-xact (list "<stdin>" 5 nil nil "Foo"))
             (posting (list -1 "Acct" "$1" nil))
             (where (ledger-marker-where-xact-is emacs-xact posting)))
        (should (= 5 (cdr where)))))))


(ert-deftest ledger-reconcile/test-marker-where-xact-is-other-file ()
  "`ledger-marker-where-xact-is' opens external files for non-stdin xacts."
  :tags '(reconcile)
  (let* ((tmp (make-temp-file "ledger-marker-" nil ".dat")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "2024/01/01 Foo\n  A  $1\n  B\n"))
          (let* ((emacs-xact (list tmp 1 nil nil "Foo"))
                 (posting (list -1 "B" "$1" nil))
                 (where (ledger-marker-where-xact-is emacs-xact posting)))
            (should (bufferp (car where)))
            (kill-buffer (car where))))
      (delete-file tmp))))


(ert-deftest ledger-reconcile/test-do-reconcile-no-uncleared ()
  "`ledger-do-reconcile' inserts the empty marker when no xacts are uncleared."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      "2024/01/01 * Foo
    Assets:Bank  $10
    Equity:Open
"
    (ledger-reconcile "Assets:Bank" '(0 "$"))
    (with-current-buffer ledger-reconcile-buffer-name
      (should (string-match-p "no uncleared entries" (buffer-string))))))


(ert-deftest ledger-reconcile/test-track-xact ()
  "`ledger-reconcile-track-xact' performs visit when this-command matches."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (forward-line 2)
    (let ((this-command 'next-line)
          (ledger-buffer-tracks-reconcile-buffer t))
      (ledger-reconcile-track-xact))
    ;; And the negative branch (no recenter).
    (let ((this-command 'self-insert-command)
          (ledger-buffer-tracks-reconcile-buffer t))
      (ledger-reconcile-track-xact))))


(ert-deftest ledger-reconcile/test-open-windows-force-bottom ()
  "`ledger-reconcile-open-windows' splits below when force-window-bottom is set."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (let ((ledger-reconcile-force-window-bottom t)
          (ledger-narrow-on-reconcile nil))
      (ledger-reconcile "Assets:Checking" '(0 "$"))
      (should (get-buffer-window ledger-reconcile-buffer-name)))))


(ert-deftest ledger-reconcile/test-interactive-prompt ()
  "Calling `ledger-reconcile' interactively asks for an account name."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (cl-letf (((symbol-function 'ledger-read-account-with-prompt)
               (lambda (_p) "Assets:Checking"))
              ((symbol-function 'ledger-read-commodity-string)
               (lambda (&rest _) '(0 "$"))))
      (call-interactively #'ledger-reconcile)
      (should (get-buffer ledger-reconcile-buffer-name)))))


(ert-deftest ledger-reconcile/test-change-target-interactive ()
  "`ledger-reconcile-change-target' calls the prompt when no target supplied."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (let ((prompted nil))
      (cl-letf (((symbol-function 'ledger-read-commodity-string)
                 (lambda (&rest _) (setq prompted t) '(0 "$"))))
        (ledger-reconcile "Assets:Checking" '(0 "$"))
        (with-current-buffer ledger-reconcile-buffer-name
          (call-interactively #'ledger-reconcile-change-target)
          (should prompted))))))


(ert-deftest ledger-reconcile/test-sort-by-commands ()
  "Each `ledger-reconcile-sort-by-*' command sets the sort key and refreshes."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (ledger-reconcile-sort-by-amount)
    (should (equal "(amount)" ledger-reconcile-sort-key))
    (ledger-reconcile-sort-by-date)
    (should (equal "(date)" ledger-reconcile-sort-key))
    (ledger-reconcile-sort-by-payee)
    (should (equal "(payee)" ledger-reconcile-sort-key))
    (ledger-reconcile-sort-by-file-order)
    (should (equal "(0)" ledger-reconcile-sort-key))))


(ert-deftest ledger-reconcile/test-display-balance-header-mode ()
  "`ledger-reconcile-display-balance-in-header-mode' toggles header-line-format."
  :tags '(reconcile)
  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$"))
    (select-window (get-buffer-window ledger-reconcile-buffer-name))
    (ledger-reconcile-display-balance-in-header-mode 1)
    (should header-line-format)
    (ledger-reconcile-display-balance-in-header-mode -1)
    (should-not header-line-format)))


(provide 'reconcile-test)

;;; reconcile-test.el ends here
