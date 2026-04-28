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

(ert-deftest ledger-navigate-uncleared ()
  :tags '(navigate)
  (ledger-tests-with-temp-file
      "2011/01/27 Book Store
    Expenses:Books                       $20.00
    Liabilities:MasterCard

2011/04/25 * Tom's Used Cars
    Expenses:Auto                    $ 5,500.00
    Assets:Checking

2011/04/27 Bookstore
    Expenses:Books                       $20.00
    Assets:Checking

2011/12/01 * Sale
    Assets:Checking                     $ 30.00
    Income:Sales"
    (ledger-navigate-next-uncleared)
    (should (looking-at-p (regexp-quote "2011/04/27 Bookstore")))
    (should-error (ledger-navigate-next-uncleared))
    (ledger-navigate-previous-uncleared)
    (should (bobp))))

(ert-deftest ledger-navigate/test-incomplete-comment-block ()
  "Incomplete comment and test blocks.
Regression test for https://github.com/ledger/ledger-mode/issues/448.
"
  :tags '(navigate regress)
  (ledger-tests-with-temp-file
      "\
2025-01-01 Payee
    Assets:Checking  $1.00
    Assets:Checking  -$1.00

"
    (goto-char (point-max))
    (insert "comment")
    (ledger-navigate-find-element-extents (point))))

(ert-deftest ledger-navigate/test-next-xact-already-at-start ()
  "`ledger-navigate-next-xact' moves on when point is already on a payee line."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "2024/01/01 Foo
    Assets:Bank  $10
    Equity:Open

2024/02/01 Bar
    Assets:Bank  $20
    Equity:Open
"
   (goto-char (point-min))             ; on first xact
   (ledger-navigate-next-xact)
   (should (looking-at-p (regexp-quote "2024/02/01 Bar")))))


(ert-deftest ledger-navigate/test-prev-xact-from-posting ()
  "`ledger-navigate-prev-xact-or-directive' from a posting line jumps to enclosing xact."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "2024/01/01 First
    Assets:Bank  $10
    Equity:Open

2024/02/01 Second
    Assets:Bank  $20
    Equity:Open
"
   ;; Position inside Second's posting line.
   (goto-char (point-min))
   (search-forward "2024/02/01")
   (forward-line 1)                     ; now on the posting line
   (ledger-navigate-prev-xact-or-directive)
   (should (looking-at-p (regexp-quote "2024/01/01 First")))))


(ert-deftest ledger-navigate/test-find-element-extents-block-comment ()
  "Comment block extents are returned correctly."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "comment
This is a comment
that spans multiple lines.
end comment

2024/01/01 Foo
    Assets:Bank  $10
    Equity:Open
"
   (goto-char (point-min))
   (forward-line 1)                     ; inside the block
   (let ((extents (ledger-navigate-find-element-extents (point))))
     (should (consp extents))
     (should (= 2 (length extents))))))


(ert-deftest ledger-navigate/test-find-directive-extents-with-comment ()
  "Comment block extents are returned correctly via `ledger-navigate-find-directive-extents'.
Uses ;-prefixed comment lines (looking-at-p comment-re branch)."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "; First comment line
; Second comment line
; Third comment line

2024/01/01 Foo
    Assets:Bank  $10
    Equity:Open
"
   (goto-char (point-min))
   ;; Position inside comment block.
   (forward-line 1)
   (save-excursion
     (let ((extents (ledger-navigate-find-element-extents (point))))
       (should (consp extents))
       (should (= 2 (length extents)))))))


(ert-deftest ledger-navigate/test-find-directive-extents-comment-after-text ()
  "Comment block preceded by a non-comment line: forward-line branch is reached."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "alias TX = Expenses:Transport
; First comment line
; Second comment line

2024/01/01 Foo
    Assets:Bank  $10
    Equity:Open
"
   (goto-char (point-min))
   (forward-line 1)                     ; on first comment line
   (let ((extents (ledger-navigate-find-element-extents (point))))
     (should (consp extents))
     (should (= 2 (length extents))))))


(ert-deftest ledger-navigate/test-block-comment ()
  "`ledger-navigate-block-comment' returns comment extents."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "; First comment line
; Second comment line
; Third comment line

2024/01/01 Foo
    Assets:Bank  $10
    Equity:Open
"
   (goto-char (point-min))
   (forward-line 1)                     ; inside the comment block
   (let ((extents (ledger-navigate-block-comment (point))))
     (should (consp extents))
     (should (= 2 (length extents)))
     ;; Both bounds should be valid buffer positions.
     (should (and (integerp (car extents)) (integerp (cadr extents)))))))


(ert-deftest ledger-navigate/test-block-comment-non-comment ()
  "`ledger-navigate-block-comment' returns single-line bounds when not on comment."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "2024/01/01 Foo
    Assets:Bank  $10
    Equity:Open
"
   (goto-char (point-min))
   (let ((extents (ledger-navigate-block-comment (point))))
     (should (consp extents))
     (should (= 2 (length extents))))))


(ert-deftest ledger-navigate/test-no-previous-uncleared ()
  "`ledger-navigate-previous-uncleared' raises a user-error when none found."
  :tags '(navigate)
  (ledger-tests-with-temp-file
   "2024/01/01 * Foo
    Assets:Bank  $10
    Equity:Open

2024/02/01 * Bar
    Assets:Bank  $20
    Equity:Open
"
   (goto-char (point-max))
   (should-error (ledger-navigate-previous-uncleared) :type 'user-error)))


(provide 'navigate-test)

;;; navigate-test.el ends here
