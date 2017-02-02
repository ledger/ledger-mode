;;; navigate-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

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
;;  Regression tests for ledger-navigate

;;; Code:
(require 'test-helper)


(ert-deftest ledger-navigate/test-001 ()
  "Regress test for Bug 441
http://bugs.ledger-cli.org/show_bug.cgi?id=441"
  :tags '(navigate regress)

  (ledger-tests-with-temp-file
   demo-ledger
   (goto-char (point-min))              ; beginning-of-buffer
   (ledger-navigate-next-xact-or-directive)
   (ledger-navigate-next-xact-or-directive)
   (should (eq 556 (point)))
   (ledger-navigate-prev-xact-or-directive)
   (should (eq 104 (point)))))


(provide 'navigate-test)

;;; navigate-test.el ends here
