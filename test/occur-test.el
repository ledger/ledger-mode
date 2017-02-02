;;; occur-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-occur

;;; Code:
(require 'test-helper)


(ert-deftest ledger-occur/test-001 ()
  "Regress test for Bug 246
http://bugs.ledger-cli.org/show_bug.cgi?id=246"
  :tags '(occur regress)

  (ledger-tests-with-temp-file
   "2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking

2011/01/05 Employer
  * Assets:Checking                 $ 2000.00
  Income:Salary
"
   (ledger-occur "Groceries")
   (should
    (equal (ledger-test-visible-buffer-string)
           "2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking
"))))


(provide 'occur-test)

;;; occur-test.el ends here
