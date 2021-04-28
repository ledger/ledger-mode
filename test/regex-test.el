;;; regex-test.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 John Wiegley <johnw AT gnu DOT org>

;; Author: Thierry <thdox AT free DOT fr>
;; Keywords: languages
;; Homepage: https://github.com/ledger/ledger-mode

;; This file is not part of GNU Emacs.

;; Package-Requires: ((emacs "25.1"))

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
;;  Regression tests for ledger-mode

;;; Code:
(require 'ledger-mode)
(require 'ert)
(require 'seq)
(require 'subr-x)

(defconst regex-test--all-ledger-regex-symbols
  (let (result)
    (mapatoms
     (lambda (symbol)
       (when (string-prefix-p "ledger-regex-" (symbol-name symbol))
         (push symbol result))))
    (seq-sort (lambda (a b) (string< (symbol-name a) (symbol-name b)))
              result))
  "List of all defined symbols that begin with `ledger-regex-'.")

(defconst regex-test--ledger-define-regexp-defined-regexps
  '("account"
    "account-kind"
    "amount"
    "amount-no-group"
    "balance-assertion"
    "code"
    "commoditized-amount"
    "commodity"
    "commodity-annotations"
    "commodity-no-group"
    "cost"
    "end-note"
    "full-account"
    "full-amount"
    "full-date"
    "full-note"
    "iso-date"
    "iterate"
    "long-space"
    "note"
    "payee"
    "post-line"
    "state"
    "xact-line"
    "year")
  "Names of the regexps defined via `ledger-define-regexp'.")

(defconst regex-test--ledger-define-regexp-defined-symbols-by-name
  (let ((hash-table (make-hash-table :test #'equal))
        ;; Try the names in reverse, so that we try longer names before their
        ;; prefixes.
        (reverse-names (reverse regex-test--ledger-define-regexp-defined-regexps)))
    ;; Add symbols in reverse so they are sorted in the correct order as we
    ;; prepend them.
    (dolist (symbol (reverse regex-test--all-ledger-regex-symbols))
      (when-let (name (seq-find (lambda (name)
                                  (string-prefix-p (concat "ledger-regex-" name "-")
                                                   (symbol-name symbol)))
                                reverse-names))
        (puthash name
                 (cons symbol (gethash name hash-table)) hash-table)))
    hash-table)
  "Mapping from regexp name to list of symbols defined for that regexp.")

(ert-deftest ledger-regex/test-001 ()
  "List the regexps defined by the `ledger-define-regexp' macro.

When new regexps defined by that macro are added, tests should be
added to this file similar to the tests for the other regexps."
  (let ((ledger-define-regexp-defined-regexps
         (seq-filter
          #'identity
          (seq-map
           (lambda (symbol)
             (when (string-match (rx (and bos "ledger-regex-" (group (* any)) "-group--count" eos))
                                 (symbol-name symbol))
               (match-string 1 (symbol-name symbol))))
           regex-test--all-ledger-regex-symbols))))
    (setq ledger-define-regexp-defined-regexps
          (seq-sort #'string< ledger-define-regexp-defined-regexps))

    (should (equal ledger-define-regexp-defined-regexps
                   regex-test--ledger-define-regexp-defined-regexps))))

(defun regex-test--dump-regex (name)
  "Return all of the variables for the regex named NAME."
  (seq-filter #'identity
              (seq-map (lambda (symbol) (when (boundp symbol) (list symbol (symbol-value symbol))))
                       (gethash name regex-test--ledger-define-regexp-defined-symbols-by-name))))

(defun regex-test--test-regexp (name expected)
  "Check that the variables associated with the regex named NAME.

In particular, the group count and index variables should match EXPECTED."
  (should (equal (regex-test--dump-regex name) expected))
  (should (= (regexp-opt-depth (symbol-value (intern (concat "ledger-" name "-regexp"))))
             (symbol-value (intern (concat "ledger-regex-" name "-group--count"))))))

(ert-deftest ledger-regex/test-account ()
  (regex-test--test-regexp
   "account"
   '((ledger-regex-account-group 1)
     (ledger-regex-account-group--count 1))))

(ert-deftest ledger-regex/test-account-kind ()
  (regex-test--test-regexp
   "account-kind"
   '((ledger-regex-account-kind-group 1)
     (ledger-regex-account-kind-group--count 1))))

(ert-deftest ledger-regex/test-amount ()
  (regex-test--test-regexp
   "amount"
   '((ledger-regex-amount-group 1)
     (ledger-regex-amount-group--count 1))))

(ert-deftest ledger-regex/test-amount-no-group ()
  (regex-test--test-regexp
   "amount-no-group"
   '((ledger-regex-amount-no-group-group--count 0))))

(ert-deftest ledger-regex/test-balance-assertion ()
  (regex-test--test-regexp
   "balance-assertion"
   '((ledger-regex-balance-assertion-group 1)
     (ledger-regex-balance-assertion-group--count 1))))

(ert-deftest ledger-regex/test-code ()
  (regex-test--test-regexp
   "code"
   '((ledger-regex-code-group 1)
     (ledger-regex-code-group--count 1))))

(ert-deftest ledger-regex/test-commoditized-amount ()
  (regex-test--test-regexp
   "commoditized-amount"
   '((ledger-regex-commoditized-amount-group 1)
     (ledger-regex-commoditized-amount-group--count 1))))

(ert-deftest ledger-regex/test-commodity ()
  (regex-test--test-regexp
   "commodity"
   '((ledger-regex-commodity-group 1)
     (ledger-regex-commodity-group--count 1))))

(ert-deftest ledger-regex/test-commodity-annotations ()
  (regex-test--test-regexp
   "commodity-annotations"
   '((ledger-regex-commodity-annotations-group--count 5)
     (ledger-regex-commodity-annotations-group-commoditized-amount 1)
     (ledger-regex-commodity-annotations-group-iso-date 2))))

(ert-deftest ledger-regex/test-commodity-no-group ()
  (regex-test--test-regexp
   "commodity-no-group"
   '((ledger-regex-commodity-no-group-group--count 0))))

(ert-deftest ledger-regex/test-cost ()
  (regex-test--test-regexp
   "cost"
   '((ledger-regex-cost-group 1)
     (ledger-regex-cost-group--count 1))))

(ert-deftest ledger-regex/test-end-note ()
  (regex-test--test-regexp
   "end-note"
   '((ledger-regex-end-note-group 1)
     (ledger-regex-end-note-group--count 1))))

(ert-deftest ledger-regex/test-full-account ()
  (regex-test--test-regexp
   "full-account"
   '((ledger-regex-full-account-group--count 2)
     (ledger-regex-full-account-group-kind 1)
     (ledger-regex-full-account-group-name 2))))

(ert-deftest ledger-regex/test-full-amount ()
  (regex-test--test-regexp
   "full-amount"
   '((ledger-regex-full-amount-group 1)
     (ledger-regex-full-amount-group--count 1))))

(ert-deftest ledger-regex/test-full-date ()
  (regex-test--test-regexp
   "full-date"
   '((ledger-regex-full-date-group--count 8)
     (ledger-regex-full-date-group-actual 1)
     (ledger-regex-full-date-group-effective 5))))

(ert-deftest ledger-regex/test-full-note ()
  (regex-test--test-regexp
   "full-note"
   '((ledger-regex-full-note-group 1)
     (ledger-regex-full-note-group--count 1))))

(ert-deftest ledger-regex/test-iso-date ()
  (regex-test--test-regexp
   "iso-date"
   '((ledger-regex-iso-date-group 1)
     (ledger-regex-iso-date-group--count 4))))

(ert-deftest ledger-regex/test-iterate ()
  (regex-test--test-regexp
   "iterate"
   '((ledger-regex-iterate-group--count 13)
     (ledger-regex-iterate-group-actual-date 2)
     (ledger-regex-iterate-group-code 11)
     (ledger-regex-iterate-group-effective-date 6)
     (ledger-regex-iterate-group-note 13)
     (ledger-regex-iterate-group-payee 12)
     (ledger-regex-iterate-group-state 10)
     (ledger-regex-iterate-group-year 1))))

(ert-deftest ledger-regex/test-long-space ()
  (regex-test--test-regexp
   "long-space"
   '((ledger-regex-long-space-group--count 0))))

(ert-deftest ledger-regex/test-note ()
  (regex-test--test-regexp
   "note"
   '((ledger-regex-note-group 1)
     (ledger-regex-note-group--count 1))))

(ert-deftest ledger-regex/test-post-line ()
  (regex-test--test-regexp
   "post-line"
   '((ledger-regex-post-line-group--count 5)
     (ledger-regex-post-line-group-account 3)
     (ledger-regex-post-line-group-account-kind 2)
     (ledger-regex-post-line-group-amount 4)
     (ledger-regex-post-line-group-note 5)
     (ledger-regex-post-line-group-state 1))))

(ert-deftest ledger-regex/test-state ()
  (regex-test--test-regexp
   "state"
   '((ledger-regex-state-group 1)
     (ledger-regex-state-group--count 1))))

(ert-deftest ledger-regex/test-xact-line ()
  (regex-test--test-regexp
   "xact-line"
   '((ledger-regex-xact-line-group--count 11)
     (ledger-regex-xact-line-group-actual-date 1)
     (ledger-regex-xact-line-group-code 10)
     (ledger-regex-xact-line-group-effective-date 5)
     (ledger-regex-xact-line-group-note 11)
     (ledger-regex-xact-line-group-state 9))))

(provide 'regex-test)

;;; regex-test.el ends here
