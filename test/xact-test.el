;;; xact-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-xact

;;; Code:
(require 'test-helper)


(ert-deftest ledger-xact/test-001 ()
  "Regress test for Bug 952+936+183
http://bugs.ledger-cli.org/show_bug.cgi?id=952
http://bugs.ledger-cli.org/show_bug.cgi?id=936
http://bugs.ledger-cli.org/show_bug.cgi?id=183"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
      "2013/05/01 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (goto-char (point-max))              ; end-of-buffer
    (ledger-add-transaction "2013/05/02 foo")
    (should
     (equal (buffer-string)
            "2013/05/01 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
" ))))


(ert-deftest ledger-xact/test-002 ()
  "Regress test for Bug 526
http://bugs.ledger-cli.org/show_bug.cgi?id=526"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
      "2013/05/01 foo
    Expenses:Foo                             10,00 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             10,00 €
    Assets:Bar
"
    (goto-char (point-max))              ; end-of-buffer
    (ledger-add-transaction "2013/05/02 foo 16,02")
    (should
     (equal (buffer-string)
            "2013/05/01 foo
    Expenses:Foo                             10,00 €
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                               16,02 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             10,00 €
    Assets:Bar
"))))


(ert-deftest ledger-xact/test-003 ()
  "Regression test for #307
https://github.com/ledger/ledger-mode/issues/307"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
      "2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (goto-char (point-max))             ; end-of-buffer
    (ledger-add-transaction "2013/05/01 foo")
    (should
     (equal (buffer-string)
            "2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"))))

(ert-deftest ledger-xact/test-004 ()
  "Add xact following all other xacts."
  :tags '(xact)

  (ledger-tests-with-temp-file
      "\
2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"

    (ledger-add-transaction "2013/05/04 foo")
    (should
     (equal (buffer-string)
            "\
2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/04 foo
    Expenses:Foo                              $10.00
    Assets:Bar
"))))

(ert-deftest ledger-xact/test-005 ()
  "Add xact with quoted/escaped arguments."
  :tags '(xact)

  (ledger-tests-with-temp-file
      "\
2013/05/01 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/02=2013/05/03 foo bar
    Expenses:Bar                            $10.00
    Assets:Foo

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
    (save-buffer)

    ;; arguments are normally word-split
    (ledger-add-transaction "2013/05/04 foo bar")
    (should
     (equal (thing-at-point 'paragraph)
            "
2013/05/04 foo
    Assets:Bar
    Assets:Bar
"))
    (revert-buffer t t)

    ;; word splitting can be inhibited by quoting...
    (ledger-add-transaction "2013/05/04 \"foo bar\"")
    (should
     (equal (thing-at-point 'paragraph)
            "
2013/05/04 foo bar
    Expenses:Bar                              $10.00
    Assets:Foo
"))
    (revert-buffer t t)

    ;; ...or backslash-escaping
    (ledger-add-transaction "2013/05/04 foo\\ bar")
    (should
     (equal (thing-at-point 'paragraph)
            "
2013/05/04 foo bar
    Expenses:Bar                              $10.00
    Assets:Foo
"))))

(provide 'xact-test)

;;; xact-test.el ends here
