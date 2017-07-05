;;; ledger-fonts.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.



;;; Commentary:
;; All of the faces for ledger mode are defined here.

;;; Code:

(require 'ledger-navigate)
(require 'ledger-regex)
(require 'ledger-state)

(defgroup ledger-faces nil "Ledger mode highlighting" :group 'ledger)

(defface ledger-font-auto-xact-face
  `((t :inherit font-lock-negation-char-face))
  "Default face for automatic transactions"
  :group 'ledger-faces)

(defface ledger-font-periodic-xact-face
  `((t :inherit font-lock-constant-face))
  "Default face for automatic transactions"
  :group 'ledger-faces)

(defface ledger-font-xact-cleared-face
  `((t :inherit ledger-font-payee-cleared-face))
  "Default face for cleared transaction"
  :group 'ledger-faces)

(defface ledger-font-xact-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending transaction"
  :group 'ledger-faces)

(defface ledger-font-payee-uncleared-face
  `((t :inherit error))
  "Default face for Ledger"
  :group 'ledger-faces)

(defface ledger-font-payee-cleared-face
  `((t :inherit shadow))
  "Default face for cleared (*) payees"
  :group 'ledger-faces)

(defface ledger-font-payee-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending (!) payees"
  :group 'ledger-faces)

(defface ledger-font-xact-highlight-face
  `((t :inherit ledger-occur-xact-face))
  "Default face for transaction under point"
  :group 'ledger-faces)

(defface ledger-font-pending-face
  `((t :inherit warning))
  "Default face for pending (!) transactions"
  :group 'ledger-faces)

(defface ledger-font-other-face
  `((t :inherit font-lock-type-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-directive-face
  `((t :inherit font-lock-preprocessor-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-account-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-note-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for note subdirectives"
  :group 'ledger-faces)

(defface ledger-font-default-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for default subdirectives"
  :group 'ledger-faces)

(defface ledger-font-price-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-apply-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-alias-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-assert-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-bucket-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-C-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for C directive"
  :group 'ledger-faces)

(defface ledger-font-capture-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-check-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-commodity-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-format-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for format subdirective"
  :group 'ledger-faces)

(defface ledger-font-D-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for D directive"
  :group 'ledger-faces)

(defface ledger-font-define-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-end-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-expr-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-fixed-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-include-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-N-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for N directive"
  :group 'ledger-faces)

(defface ledger-font-payee-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-uuid-directive-face
  `((t :inherit ledger-font-directive-face))
  "Face for uuid subdirectives"
  :group 'ledger-faces)

(defface ledger-font-tag-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-timeclock-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for timeclock I,i,O,o,b,h directives"
  :group 'ledger-faces)

(defface ledger-font-year-directive-face
  `((t :inherit ledger-font-directive-face))
  "Default face for other transactions"
  :group 'ledger-faces)

(defface ledger-font-posting-account-face
  `((t :inherit ledger-font-default-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-cleared-face
  `((t :inherit ledger-font-payee-cleared-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-cleared-face
  `((t :inherit ledger-font-posting-account-cleared-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-account-pending-face
  `((t :inherit ledger-font-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-pending-face
  `((t :inherit ledger-font-posting-account-pending-face))
  "Face for Ledger accounts"
  :group 'ledger-faces)

(defface ledger-font-posting-amount-face
  `((t :inherit font-lock-constant-face ))
  "Face for Ledger amounts"
  :group 'ledger-faces)

(defface ledger-font-posting-date-face
  `((t :inherit font-lock-keyword-face))
  "Face for Ledger dates"
  :group 'ledger-faces)

(defface ledger-occur-narrowed-face
  `((t :inherit font-lock-comment-face :invisible t))
  "Default face for Ledger occur mode hidden transactions"
  :group 'ledger-faces)

(defface ledger-occur-xact-face
  `((t :inherit highlight))
  "Default face for Ledger occur mode shown transactions"
  :group 'ledger-faces)

(defface ledger-font-comment-face
  `((t :inherit font-lock-comment-face))
  "Face for Ledger comments"
  :group 'ledger-faces)

(defface ledger-font-reconciler-uncleared-face
  `((t :inherit ledger-font-payee-uncleared-face))
  "Default face for uncleared transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-cleared-face
  `((t :inherit ledger-font-payee-cleared-face))
  "Default face for cleared (*) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-reconciler-pending-face
  `((t :inherit ledger-font-pending-face))
  "Default face for pending (!) transactions in the reconcile window"
  :group 'ledger-faces)

(defface ledger-font-report-clickable-face
  `((t))
  "Face applied to clickable entries in the report window"
  :group 'ledger-faces)

(defface ledger-font-code-face
  `((t :inherit default))
  "Face for Ledger codes"
  :group 'ledger-faces)

(defun ledger-font-face-by-state (num faces)
  "Choose one of three faces depending on transaction state.
NUM specifies a match group containing the state.
FACES has the form (CLEARED PENDING OTHER).
Return CLEARED if that group specifies a cleared transaction,
PENDING if pending, and OTHER if none of the above."
  (let ((state (save-match-data (ledger-state-from-string (match-string num)))))
    (cond ((eq state 'cleared) (nth 0 faces))
          ((eq state 'pending) (nth 1 faces))
          (t (nth 2 faces)))))

(defun ledger-font-subdirectives (subdirectives)
  "Construct anchored highlighters for subdirectives.

Each element of SUBDIRECTIVES should have the form (MATCHER
SUBEXP-HIGHLIGHTERS…). The result will be a list of elements of
the form (MATCHER PRE-FORM POST-FORM SUBEXP-HIGHLIGHTERS) with
PRE-FORM and POST-FORM set to appropriate values.

See `font-lock-keywords' for the full description."

  (mapcar (lambda (item)
            `(,(car item)
              (save-excursion
                (save-match-data
                  (ledger-navigate-end-of-xact))
                (point))
              (goto-char (match-end 0))
              ,@(cdr item)))
          subdirectives))

(defvar ledger-font-lock-keywords
  `(("^[;#%|*].*$" . 'ledger-font-comment-face)
    ("^account\\>.*$"
     (0 'ledger-font-account-directive-face)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(note\\>.*\\)" (1 'ledger-font-note-directive-face))
          ("^[ \t]+\\(alias\\>.*\\)" (1 'ledger-font-alias-directive-face))
          ("^[ \t]+\\(payee\\>.*\\)" (1 'ledger-font-payee-directive-face))
          ("^[ \t]+\\(check\\>.*\\)" (1 'ledger-font-check-directive-face))
          ("^[ \t]+\\(assert\\>.*\\)" (1 'ledger-font-assert-directive-face))
          ("^[ \t]+\\(eval\\>.*\\)" (1 'ledger-font-expr-directive-face))
          ("^[ \t]+\\(default\\>.*\\)" (1 'ledger-font-default-directive-face)))))
    ("^alias\\>.*$" . 'ledger-font-alias-directive-face)
    ("^apply\\>.*$" . 'ledger-font-apply-directive-face)
    ("^assert\\>.*$" . 'ledger-font-assert-directive-face)
    ("^\\(?:bucket\\|A\\)\\>.*$" . 'ledger-font-bucket-directive-face)
    ("^C\\>.*$" . 'ledger-font-C-directive-face)
    ("^capture\\>.*$" . 'ledger-font-capture-directive-face)
    ("^check\\>.*$" . 'ledger-font-check-directive-face)
    (,(concat "^\\(?:comment\\|test\\)\\>"
              "[^\0]*?\n"
              "end[[:blank:]]+\\(?:comment\\|test\\)\\>.*\n")
     . 'ledger-font-comment-face)
    ("^commodity\\>.*$"
     (0 'ledger-font-commodity-directive-face)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(note\\>.*\\)" (1 'ledger-font-note-directive-face))
          ("^[ \t]+\\(format\\>.*\\)" (1 'ledger-font-format-directive-face))
          ("^[ \t]+\\(nomarket\\>.*\\)" (1 'ledger-font-N-directive-face))
          ("^[ \t]+\\(default\\>.*\\)" (1 'ledger-font-default-directive-face)))))
    ("^D\\>.*$" . 'ledger-font-D-directive-face)
    ("^\\(?:define\\|def\\)\\>.*$" . 'ledger-font-define-directive-face)
    ;; FIXME: this matches “end” and “endfixed” but also “endoscopy”
    ("^end.*$" . 'ledger-font-end-directive-face)
    ("^expr\\>.*$" . 'ledger-font-expr-directive-face)
    ("^fixed\\>.*$" . 'ledger-font-fixed-directive-face)
    ("^include\\>.*$" . 'ledger-font-include-directive-face)
    ("^N\\>.*$" . 'ledger-font-N-directive-face)
    ("^payee\\>.*$"
     (0 'ledger-font-payee-directive-face)
     ,@(ledger-font-subdirectives
        '(("^[ \t]+\\(;.*\\)" (1 'ledger-font-comment-face))
          ("^[ \t]+\\(alias\\>.*\\)" (1 'ledger-font-alias-directive-face))
          ("^[ \t]+\\(uuid\\>.*\\)" (1 'ledger-font-uuid-directive-face)))))
    ("^P\\>.*$" . 'ledger-font-price-directive-face)
    ("^tag\\>.*$" . 'ledger-font-tag-directive-face)
    ("^[IiOobh]\\>.*$" . 'ledger-font-timeclock-directive-face)
    ("^\\(?:year\\|Y\\)\\>.*$" . 'ledger-font-year-directive-face)

    ("^=.*\\(?:\n[ \t]+.*\\)*" . 'ledger-font-auto-xact-face)
    ("^~.*\\(?:\n[ \t]+.*\\)*" . 'ledger-font-periodic-xact-face)
    (,(lambda (limit)
        (when ledger-fontify-xact-state-overrides
          (re-search-forward
           (concat "^[[:digit:]][^ \t\n]*"   ; date
                   "[ \t]+\\([*!]\\)"        ; mark, subexp 1
                   ".*"                      ; rest of header
                   "\\(?:\n[ \t]+.*\\)*"     ; postings
                   )
           limit t)))
     (0 (ledger-font-face-by-state 1 '(ledger-font-xact-cleared-face
                                       ledger-font-xact-pending-face))))
    (,(concat "^\\([[:digit:]][^ \t\n]*\\)" ; date, subexp 1
              ledger-xact-after-date-regex) ; mark 2, code 3, desc 4, comment 5
     (1 'ledger-font-posting-date-face)
     (3 'ledger-font-code-face nil :lax)
     (4 (ledger-font-face-by-state 2 '(ledger-font-payee-cleared-face
                                       ledger-font-payee-pending-face
                                       ledger-font-payee-uncleared-face)))
     (5 'ledger-font-comment-face nil :lax)
     ,@(ledger-font-subdirectives
        `(("^[ \t]+;.*"
           (0 'ledger-font-comment-face))
          (,ledger-posting-regex ; state and account 1, state 2, amount 4, comment 5
           (1 (ledger-font-face-by-state 2 '(ledger-font-posting-account-cleared-face
                                             ledger-font-posting-account-pending-face
                                             ledger-font-posting-account-face))
              nil :lax)
           (4 (ledger-font-face-by-state 2 '(ledger-font-posting-amount-cleared-face
                                             ledger-font-posting-amount-pending-face
                                             ledger-font-posting-amount-face)))
           (5 'ledger-font-comment-face))))))
  "Expressions to highlight in Ledger mode.")



(provide 'ledger-fonts)

;;; ledger-fonts.el ends here
