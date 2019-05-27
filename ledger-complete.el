;;; ledger-complete.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;; Functions providing payee and account auto complete.

(require 'cl-lib)

;; Emacs 24.3 compatibility
(defun ledger-string-greaterp (string1 string2)
  "Return non-nil if STRING1 is greater than STRING2 in lexicographic order.
Case is significant."
  (string-lessp string2 string1))

;; In-place completion support

;;; Code:
(require 'ledger-context)
(require 'ledger-xact)
(require 'ledger-schedule)

(defcustom ledger-accounts-file nil
  "The path to an optional file in which all accounts are used or declared.
This file will then be used as a source for account name completions."
  :type 'file
  :group 'ledger)

(defcustom ledger-complete-in-steps nil
  "When non-nil, `ledger-complete-at-point' completes account names in steps.
If nil, full account names are offer for completion."
  :type 'boolean
  :group 'ledger
  :package-version '(ledger-mode . "2019-05-27"))

(defun ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  ;; FIXME: We don't use pcomplete anymore.
  ;; This is more complex than it appears
  ;; to need, so that it can work with pcomplete.  See
  ;; pcomplete-parse-arguments-function for details
  (let* ((begin (save-match-data
                  (if (looking-back (concat "^\\(" ledger-iso-date-regexp "=\\|\\)"
                                            ledger-incomplete-date-regexp) nil)
                      (match-end 1)
                    (save-excursion
                      (ledger-thing-at-point) ;; leave point at beginning of thing under point
                      (point)))))
         (end (point))
         begins args)
    ;; to support end of line metadata
    (save-excursion
      (when (search-backward ";"
                             (line-beginning-position) t)
        (setq begin (match-beginning 0))))
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
        (skip-chars-forward " \t\n")
        (setq begins (cons (point) begins))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) end)
                         args)))
      (cons (reverse args) (reverse begins)))))


(defun ledger-payees-in-buffer ()
  "Scan buffer and return list of all payees."
  (let ((origin (point))
        payees-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              ledger-payee-any-status-regex nil t)  ;; matches first line
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq payees-list (cons (match-string-no-properties 3)
                                  payees-list)))))  ;; add the payee
    ;; to the list
    (delete-dups (sort payees-list #'string-lessp))))

(defun ledger-accounts-deduplicate-sorted (l)
  "Remove duplicates from a sorted list of strings L."
  (let ((current l))
    (while (consp current)
      (if (string= (car current) (cadr current))
          (setcdr current (cddr current))
        (pop current)))
    l))

(defun ledger-accounts-list-in-buffer ()
  "Return a list of all known account names in the current buffer as strings.
Considers both accounts listed in postings and those declared with \"account\" directives."
  (save-excursion
    (goto-char (point-min))
    (let (results)
      (while (re-search-forward ledger-account-name-or-directive-regex nil t)
        (setq results (cons (match-string-no-properties 2) results)))
      (ledger-accounts-deduplicate-sorted
       (sort results #'ledger-string-greaterp)))))

(defun ledger-accounts-list ()
  "Return a list of all known account names as strings.
Looks in `ledger-accounts-file' if set, otherwise the current buffer."
  (if ledger-accounts-file
      (let ((f ledger-accounts-file))
        (with-temp-buffer
          (insert-file-contents f)
          (ledger-accounts-list-in-buffer)))
    (ledger-accounts-list-in-buffer)))

(defun ledger-find-accounts-in-buffer ()
  (interactive)
  (let ((account-tree (list t))
        (account-elements nil)
        (prefix ""))
    (save-excursion
      (goto-char (point-min))

      (dolist (account
               (cl-remove-if-not (lambda (c) (string-prefix-p prefix c nil))
                                 (ledger-accounts-list)))
        (let ((root account-tree))
          (setq account-elements
                (split-string
                 account ":"))
          (while account-elements
            (let ((xact (assoc (car account-elements) root)))
              (if xact
                  (setq root (cdr xact))
                (setq xact (cons (car account-elements) (list t)))
                (nconc root (list xact))
                (setq root (cdr xact))))
            (setq account-elements (cdr account-elements))))))
    account-tree))

(defun ledger-accounts-tree ()
  "Return a tree of all accounts in the buffer."
  (let* ((current (caar (ledger-parse-arguments)))
         (elements (and current (split-string current ":")))
         (root (ledger-find-accounts-in-buffer))
         (prefix nil))
    (while (cdr elements)
      (let ((xact (assoc (car elements) root)))
        (if xact
            (setq prefix (concat prefix (and prefix ":")
                                 (car elements))
                  root (cdr xact))
          (setq root nil elements nil)))
      (setq elements (cdr elements)))
    (setq root (delete (list (car elements) t) root))
    (and root
         (sort
          (mapcar (function
                   (lambda (x)
                     (let ((term (if prefix
                                     (concat prefix ":" (car x))
                                   (car x))))
                       (if (> (length (cdr x)) 1)
                           (concat term ":")
                         term))))
                  (cdr root))
          'string-lessp))))

(defun ledger-complete-date (month-string day-string)
  "Complete a date."
  (let*
      ((now (current-time))
       (decoded (decode-time now))
       (this-month (nth 4 decoded))
       (this-year (nth 5 decoded))
       (last-month (if (> this-month 1) (1- this-month) 12))
       (last-year (1- this-year))
       (last-month-year (if (> this-month 1) this-year last-year))
       (month (and month-string
                   (string-to-number month-string)))
       (day (string-to-number day-string))
       (dates (list (encode-time 0 0 0 day (or month this-month) this-year)
                    (if month
                        (encode-time 0 0 0 day month last-year)
                      (encode-time 0 0 0 day last-month last-month-year)))))
    (lambda (_string _predicate _all)
      (concat (ledger-format-date
               (cl-find-if (lambda (date) (not (time-less-p now date))) dates))
              (and (= (point) (line-end-position)) " ")))))

(defun ledger-complete-effective-date
    (tx-year-string tx-month-string tx-day-string
                    month-string day-string)
  "Complete an effective date."
  (let*
      ((tx-year (string-to-number tx-year-string))
       (tx-month (string-to-number tx-month-string))
       (tx-day (string-to-number tx-day-string))
       (tx-date (encode-time 0 0 0 tx-day tx-month tx-year))
       (next-month (if (< tx-month 12) (1+ tx-month) 1))
       (next-year (1+ tx-year))
       (next-month-year (if (< tx-month 12) tx-year next-year))
       (month (and month-string
                   (string-to-number month-string)))
       (day (string-to-number day-string))
       (dates (list (encode-time 0 0 0 day (or month tx-month) tx-year)
                    (if month
                        (encode-time 0 0 0 day month next-year)
                      (encode-time 0 0 0 day next-month next-month-year)))))
    (lambda (_string _predicate _all)
      (concat (ledger-format-date
               (cl-find-if (lambda (date) (not (time-less-p date tx-date))) dates))
              (and (= (point) (line-end-position)) " ")))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point."
  (let ((end (point))
        start collection)
    (cond (;; Date
           (looking-back (concat "^" ledger-incomplete-date-regexp) (line-beginning-position))
           (progn (setq start (match-beginning 0))
                  (setq collection (ledger-complete-date (match-string 1) (match-string 2)))))
          (;; Effective dates
           (looking-back (concat "^" ledger-iso-date-regexp "=" ledger-incomplete-date-regexp)
                         (line-beginning-position))
           (progn (setq start (line-beginning-position))
                  (setq collection (ledger-complete-effective-date
                                    (match-string 2) (match-string 3) (match-string 4)
                                    (match-string 5) (match-string 6)))))
          (;; Payees
           (eq (save-excursion (ledger-thing-at-point)) 'transaction)
           (progn (setq start (save-excursion (backward-word) (point)))
                  (setq collection #'ledger-payees-in-buffer)))
          ((not (bolp)) ;; Accounts
           (progn (setq start (save-excursion (back-to-indentation) (point)))
                  (if ledger-complete-in-steps
                      (setq collection #'ledger-accounts-tree)
                    (setq collection #'ledger-accounts-list-in-buffer)))))
    (when collection
      (list start end (if (functionp collection)
                          (completion-table-dynamic (lambda (_) (funcall collection)))
                        collection)))))

(defun ledger-trim-trailing-whitespace (str)
  (replace-regexp-in-string "[ \t]*$" "" str))

(defun ledger-fully-complete-xact ()
  "Completes a transaction if there is another matching payee in the buffer.
Does not use ledger xact"
  (interactive)
  (let* ((name (ledger-trim-trailing-whitespace (caar (ledger-parse-arguments))))
         (rest-of-name name)
         xacts)
    (save-excursion
      (when (eq 'transaction (ledger-thing-at-point))
        (delete-region (point) (+ (length name) (point)))
        ;; Search backward for a matching payee
        (when (re-search-backward
               (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.*"
                       (regexp-quote name) ".*\\)" ) nil t)
          (setq rest-of-name (match-string 3))
          ;; Start copying the postings
          (forward-line)
          (while (looking-at ledger-account-any-status-regex)
            (setq xacts (cons (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))
                              xacts))
            (forward-line))
          (setq xacts (nreverse xacts)))))
    ;; Insert rest-of-name and the postings
    (when xacts
      (save-excursion
        (insert rest-of-name ?\n)
        (while xacts
          (insert (car xacts) ?\n)
          (setq xacts (cdr xacts))))
      (forward-line)
      (goto-char (line-end-position))
      (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
          (goto-char (match-end 0))))))

(provide 'ledger-complete)

;;; ledger-complete.el ends here
