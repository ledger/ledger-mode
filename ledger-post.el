;;; ledger-post.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;; Utility functions for dealing with postings.

(require 'ledger-regex)
(require 'ledger-navigate)

(declare-function calc-renumber-stack "calc" ())
(declare-function ledger-add-commodity "ledger-commodities" (c1 c2))
(declare-function ledger-commodity-to-string "ledger-commodities" (c1))
(declare-function ledger-negate-commodity "ledger-commodities" (c))
(declare-function ledger-split-commodity-string "ledger-commodities" (str))
(declare-function ledger-string-to-number "ledger-commodities" (str &optional decimal-comma))

;;; Code:

(defgroup ledger-post nil
  "Options for controlling how Ledger-mode deals with postings and completion"
  :group 'ledger)

(defcustom ledger-post-account-alignment-column 4
  "The column Ledger-mode attempts to align accounts to."
  :type 'integer
  :group 'ledger-post
  :safe 'integerp)

(defcustom ledger-post-amount-alignment-column 52
  "The column Ledger-mode attempts to align amounts to."
  :type 'integer
  :group 'ledger-post
  :safe 'integerp)

(defcustom ledger-post-amount-alignment-at :end
  "Position at which the amount is aligned.

Can be :end to align on the last number of the amount (can be
followed by unaligned commodity) or :decimal to align at the
decimal separator."
  :type '(radio (const :tag "align at the end of amount" :end)
                (const :tag "align at the decimal separator" :decimal))
  :group 'ledger-post
  :safe (lambda (x) (memq x '(:end :decimal))))

(defcustom ledger-post-auto-align t
  "When non-nil, realign post amounts when indenting or completing."
  :type 'boolean
  :group 'ledger-post
  :package-version '(ledger-mode . "4.0.0")
  :safe 'booleanp)

(defun ledger-next-amount (&optional end)
  "Move point to the next amount, as long as it is not past END.
Return the width of the amount field as an integer and leave
point at beginning of the commodity."
  ;;(beginning-of-line)
  (let ((case-fold-search nil))
    (when (re-search-forward ledger-amount-regex end t)
      (goto-char (match-beginning 0))
      (skip-syntax-forward " ")
      (cond
       ((eq ledger-post-amount-alignment-at :end)
        (- (or (match-end 4) (match-end 3)) (point)))
       ((eq ledger-post-amount-alignment-at :decimal)
        (- (match-end 3) (point)))))))

(defun ledger-next-account (&optional end)
  "Move to the beginning of the posting, or status marker.
Return the column of the beginning of the account and leave point
at beginning of account.
Looks only as far as END, if supplied, otherwise `point-max'."
  (let ((end (or end (point-max))))
    (if (> end (point))
        (when (re-search-forward ledger-account-any-status-regex (1+ end) t)
          ;; the 1+ is to make sure we can catch the newline
          (if (match-beginning 1)
              (goto-char (match-beginning 1))
            (goto-char (match-beginning 2)))
          (current-column)))))

(defun ledger-post-align-xact (pos)
  "Align all the posting in the xact at POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (ledger-post-align-postings (car bounds) (cadr bounds))))

(defun ledger-post-align-postings (beg end)
  "Align all accounts and amounts between BEG and END.
The current region is used, or, if no region, the current line."
  (interactive "r")
  (save-match-data
    (save-excursion
      (let ((inhibit-modification-hooks t)
            ;; Extend region to whole lines
            (beg (save-excursion (goto-char beg) (line-beginning-position)))
            (end (save-excursion (goto-char end) (move-end-of-line 1) (point-marker))))
        (untabify beg end)
        (goto-char beg)
        (while (< (point) end)
          (when (looking-at-p " ")
            ;; fix spaces at beginning of line:
            (just-one-space ledger-post-account-alignment-column)
            ;; fix spaces before amount if any:
            (when (re-search-forward "\t\\|  \\| \t" (line-end-position) t)
              (goto-char (match-beginning 0))
              (let ((acct-end-column (current-column))
                    (amt-width (ledger-next-amount (line-end-position)))
                    amt-adjust)
                (when amt-width
                  (if (/= 0 (setq amt-adjust (- (if (> (- ledger-post-amount-alignment-column amt-width)
                                                       (+ 2 acct-end-column))
                                                    ledger-post-amount-alignment-column ;;we have room
                                                  (+ acct-end-column 2 amt-width))
                                                amt-width
                                                (current-column))))
                      (if (> amt-adjust 0)
                          (insert (make-string amt-adjust ? ))
                        (delete-char amt-adjust)))))))
          (forward-line 1))))))

(defun ledger-indent-line ()
  "Indent the current line."
  ;; Ensure indent if the previous line was indented
  (let ((indent-level (save-excursion (if (and (zerop (forward-line -1))
                                               (memq (ledger-thing-at-point) '(transaction posting)))
                                          ledger-post-account-alignment-column
                                        0))))
    (unless (= (current-indentation) indent-level)
      (back-to-indentation)
      (delete-horizontal-space t)
      (indent-to indent-level)))
  (when ledger-post-auto-align
    (ledger-post-align-postings (line-beginning-position) (line-end-position))))

(defun ledger-post-align-dwim ()
  "Align all the posting of the current xact or the current region.

If the point is in a comment, fill the comment paragraph as
regular text."
  (interactive)
  (cond
   ((nth 4 (syntax-ppss))
    (call-interactively 'ledger-post-align-postings)
    (fill-paragraph))
   ((use-region-p) (call-interactively 'ledger-post-align-postings))
   (t (call-interactively 'ledger-post-align-xact))))

(defun ledger-post-edit-amount ()
  "Call `calc' and push the amount in the posting to the top of stack, if any.

In the calc buffer, press y to use the top value in the stack as
the amount and return to ledger."
  (interactive)
  (beginning-of-line)
  (when (re-search-forward ledger-post-line-regexp (line-end-position) t)
    (goto-char (match-end ledger-regex-post-line-group-account)) ;; go to the end of the account
    ;; determine if there is an amount to edit
    (if (re-search-forward ledger-amount-regexp (line-end-position) t)
        (let ((val-string (match-string 0)))
          (goto-char (match-beginning 0))
          (delete-region (match-beginning 0) (match-end 0))
          (push-mark (point) 'nomsg)
          (calc)
          ;; edit the amount, first removing thousands separators and converting
          ;; decimal commas to calc's input format
          (calc-eval (number-to-string (ledger-string-to-number val-string)) 'push)
          (calc-renumber-stack))
      ;; make sure there are two spaces after the account name and go to calc
      (if (search-backward "  " (- (point) 3) t)
          (end-of-line)
        (insert "  "))
      (push-mark (point) 'nomsg)
      (calc))))

(defun ledger-post-xact-total ()
  "Return (TOTAL . MISSING-POSITIONS) for the transaction at point.

TOTAL is a commoditized amount representing the total amount of
the postings in the transaction.

MISSING-POSITIONS is a list of positions in the buffer where the
transaction do not have an amount specified (such postings do not
contribute to TOTAL).  Specifically, the positions are at the end
of the account name on such posting lines.

Error if the commodities do not match."
  (save-excursion
    (pcase-let ((`(,begin ,end) (ledger-navigate-find-xact-extents (point))))
      (goto-char begin)
      (cl-loop
       while (re-search-forward ledger-post-line-regexp end t)
       for account-end = (match-end ledger-regex-post-line-group-account)
       for amount-string = (when-let ((amount-string (match-string ledger-regex-post-line-group-amount)))
                             (unless (string-empty-p (string-trim amount-string))
                               amount-string))
       if (not amount-string)
       collect account-end into missing-positions
       else
       collect (ledger-split-commodity-string amount-string) into amounts
       finally return (cons (if amounts
                                (cl-reduce #'ledger-add-commodity amounts)
                              '(0 nil))
                            missing-positions)))))

(defun ledger-post-fill ()
  "Find a posting with no amount and insert it.

Even if ledger allows for one missing amount per transaction, you
might want to insert it anyway."
  (interactive)
  (pcase-let* ((`(,total . ,missing-positions) (ledger-post-xact-total))
               (missing-amount (ledger-negate-commodity total))
               (amounts-balance (< (abs (car missing-amount)) 0.0001)))
    (pcase missing-positions
      ('() (unless amounts-balance
             (user-error "Postings do not balance, but no posting to fill")))
      (`(,missing-pos)
       (if amounts-balance
           (user-error "Missing amount but amounts balance already")
         (goto-char missing-pos)
         (insert "  " (ledger-commodity-to-string missing-amount))
         (ledger-post-align-xact (point))))
      (_ (user-error "More than one posting with missing amount")))))

(provide 'ledger-post)



;;; ledger-post.el ends here
