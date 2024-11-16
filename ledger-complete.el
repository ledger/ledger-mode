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
(eval-when-compile
  (require 'subr-x))

;; In-place completion support

;;; Code:
(require 'ledger-context)
(require 'ledger-xact)
(require 'ledger-schedule)

(defcustom ledger-accounts-file nil
  "The path to an optional file in which all accounts are used or declared.
This file will then be used as a source for account name
completions instead of the current file.
See ledger's \"account\" directive."
  :type '(choice (const :tag "Use current buffer for completion" nil)
                 file)
  :group 'ledger
  :safe #'string-or-null-p)

(defcustom ledger-payees-file nil
  "The path to an optional file in which all payees are used or declared.
This file will then be used as a source for payee name
completions instead of the current file.
See ledger's \"payee\" directive."
  :type '(choice (const :tag "Use current buffer for completion" nil)
                 file)
  :group 'ledger
  :safe #'string-or-null-p)

(defcustom ledger-accounts-exclude-function nil
  "Function to exclude accounts from completion.
Should be a predicate function that accepts one argument, an
element of `ledger-accounts-list-in-buffer'."
  :type '(choice (const :tag "Do not exclude any accounts from completion" nil)
                 function)
  :group 'ledger
  :package-version '(ledger-mode . "2019-08-14"))

(defcustom ledger-complete-in-steps nil
  "When non-nil, `ledger-complete-at-point' completes account names in steps.
If nil, full account names are offered for completion."
  :type 'boolean
  :group 'ledger
  :package-version '(ledger-mode . "4.0.0"))

(defun ledger-payees-in-buffer ()
  "Scan buffer and return list of all payees."
  (let ((origin (point))
        payees-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ledger-payee-name-or-directive-regex nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (push (or (match-string-no-properties 1) (match-string-no-properties 2))
                payees-list))))
    ;; to the list
    (sort (delete-dups payees-list) #'string-lessp)))

(defun ledger-payees-list ()
  "Return a list of all known account names as strings.
Looks in `ledger-payees-file' if set, otherwise the current buffer."
  (if ledger-payees-file
      (let ((f ledger-payees-file))
        (with-temp-buffer
          (insert-file-contents f)
          (ledger-payees-in-buffer)))
    (ledger-payees-in-buffer)))

(defun ledger-accounts-in-buffer ()
  "Return an alist of accounts in the current buffer.
The `car' of each element is the account name and the `cdr' is an
alist where the key is a subdirective such as \"assert\" and the
value (if any) is the associated data.  In other words, if you've
declared an account like so:

account Assets:Checking
    assert commodity == \"$\"
    default

Then one of the elements this function returns will be
\(\"Assets:Checking\"
  (\"default\")
  (\"assert\" . \"commodity == \"$\"\"))"
  (save-excursion
    (goto-char (point-min))
    (let (account-list
          (seen (make-hash-table :test #'equal :size 1)))
      ;; First, consider accounts declared with "account" directives, which may or
      ;; may not have associated data. The data is on the following lines up to a
      ;; line not starting with whitespace.
      (while (re-search-forward ledger-account-directive-regex nil t)
        (let ((account (match-string-no-properties 1))
              (lines (buffer-substring-no-properties
                      (point)
                      (progn (ledger-navigate-next-xact-or-directive)
                             (point))))
              data)
          (dolist (d (split-string lines "\n"))
            (setq d (string-trim d))
            (unless (string= d "")
              (if (string-match " " d)
                  (push (cons (substring d 0 (match-beginning 0))
                              (substring d (match-end 0) nil))
                        data)
                (push (cons d nil) data))))
          (push (cons account data) account-list)
          (puthash account t seen)))
      ;; Next, gather all accounts declared in postings
      (unless
          ;; FIXME: People who have set `ledger-flymake-be-pedantic' to non-nil
          ;; probably don't want accounts from postings, just those declared
          ;; with directives.  But the name is a little misleading.  Should we
          ;; make a ledger-mode-be-pedantic and use that instead?
          (bound-and-true-p ledger-flymake-be-pedantic)
        (ledger-xact-iterate-transactions
         (lambda (_pos _date _state _payee)
           (let ((end (save-excursion (ledger-navigate-end-of-xact))))
             (while (re-search-forward ledger-account-any-status-regex end t)
               (let ((account (match-string-no-properties 1)))
                 (unless (gethash account seen)
                   (puthash account t seen)
                   (push (cons account nil) account-list))))))))
      (sort account-list (lambda (a b) (string-lessp (car a) (car b)))))))

(defun ledger-accounts-list-in-buffer ()
  "Return a list of all known account names in the current buffer as strings.
Considers both accounts listed in postings and those declared
with \"account\" directives."
  (let ((accounts (ledger-accounts-in-buffer)))
    (when ledger-accounts-exclude-function
      (setq accounts (cl-remove-if ledger-accounts-exclude-function accounts)))
    (mapcar #'car accounts)))

(defun ledger-accounts-list ()
  "Return a list of all known account names as strings.
Looks in `ledger-accounts-file' if set, otherwise the current buffer."
  (if ledger-accounts-file
      (let ((f ledger-accounts-file))
        (with-temp-buffer
          (insert-file-contents f)
          (ledger-accounts-list-in-buffer)))
    (ledger-accounts-list-in-buffer)))

(defun ledger-accounts-tree ()
  "Return a tree of all accounts in the buffer.

Each node in the tree is a list (t . CHILDREN), where CHILDREN is
an alist (ACCOUNT-ELEMENT . NODE)."
  (let ((account-tree (list t)))
    (dolist (account (ledger-accounts-list) account-tree)
      (let ((root account-tree)
            (account-elements (split-string account ":")))
        (dolist (element account-elements)
          (let ((node (assoc element root)))
            (unless node
              (setq node (cons element (list t)))
              (nconc root (list node)))
            (setq root (cdr node))))))))

(defun ledger-complete-account-next-steps ()
  "Return a list of next steps for the account prefix at point."
  ;; FIXME: This function is called from `ledger-complete-at-point' which
  ;; already knows the bounds of the account name to complete.  Computing it
  ;; again here is wasteful.
  (let* ((current (buffer-substring
                   (save-excursion
                     (unless (eq 'posting (ledger-thing-at-point))
                       (error "Not on a posting line"))
                     (point))
                   (point)))
         (elements (and current (split-string current ":")))
         (root (ledger-accounts-tree))
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

(defvar ledger-complete--current-time-for-testing nil
  "Internal, used for testing only.")

(defun ledger-complete-date (month-string day-string date-at-eol-p)
  "Complete a date."
  (let* ((now (or ledger-complete--current-time-for-testing (current-time)))
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
    (let ((collection
           (list (concat (ledger-format-date
                          (cl-find-if (lambda (date) (not (time-less-p now date))) dates))
                         (when date-at-eol-p " ")))))
      (lambda (string predicate action)
        (if (eq action 'metadata)
            '(metadata (category . ledger-date))
          (complete-with-action action collection string predicate))))))

(defun ledger-complete-effective-date
    (tx-year-string tx-month-string tx-day-string
                    month-string day-string
                    date-at-eol-p)
  "Complete an effective date."
  (let* ((tx-year (string-to-number tx-year-string))
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
    (let ((collection
           (list (concat (ledger-format-date
                          (cl-find-if (lambda (date) (not (time-less-p date tx-date))) dates))
                         (when date-at-eol-p " ")))))
      (lambda (string predicate action)
        (if (eq action 'metadata)
            '(metadata (category . ledger-date))
          (complete-with-action action collection string predicate))))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point."
  (let ((end (point))
        start collection
        realign-after
        delete-suffix)
    (cond (;; Date
           (save-excursion
             (skip-chars-forward "0-9/-")
             (looking-back (concat "^" ledger-incomplete-date-regexp) (line-beginning-position)))
           (setq collection (ledger-complete-date (match-string 1)
                                                  (match-string 2)
                                                  (= (line-end-position) (match-end 0)))
                 start (match-beginning 0)
                 ;; FIXME: This delete-suffix-post-completion behavior is weird
                 ;; and doesn't integrate well with different completion styles.
                 ;; For example, it breaks partial-completion's behavior when in
                 ;; the middle of the identifier.
                 ;;
                 ;; Instead, it should be implemented as an alternative
                 ;; completion style which is like emacs22 but discards the
                 ;; suffix.  Or perhaps ledger-mode might rebind TAB to some key
                 ;; that deletes the account at point and then calls completion.
                 delete-suffix (save-match-data
                                 (when (looking-at (rx (one-or-more (or digit (any ?/ ?-)))))
                                   (length (match-string 0))))))
          (;; Effective dates
           (save-excursion
             (skip-chars-forward "0-9/-")
             (looking-back (concat "^" ledger-iso-date-regexp "=" ledger-incomplete-date-regexp)
                           (line-beginning-position)))
           (setq start (line-beginning-position))
           (setq collection (ledger-complete-effective-date
                             (match-string 2) (match-string 3) (match-string 4)
                             (match-string 5) (match-string 6)
                             (= (line-end-position) (match-end 0)))))
          (;; Payees
           (eq 'transaction
               (save-excursion
                 (prog1 (ledger-thing-at-point)
                   (setq start (point)))))
           (setq collection (cons 'nullary #'ledger-payees-list)))
          (;; Accounts
           (save-excursion
             (back-to-indentation)
             (skip-chars-forward "([") ;; for virtual accounts
             (setq start (point)))
           (setq delete-suffix (save-excursion
                                 (when (search-forward-regexp
                                        (rx (or eol (any "\t])") (repeat 2 space)))
                                        (line-end-position) t)
                                   (- (match-beginning 0) end)))
                 realign-after t
                 collection (cons 'nullary
                                  (if ledger-complete-in-steps
                                      #'ledger-complete-account-next-steps
                                    #'ledger-accounts-list)))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (pcase collection
                ;; `func-arity' isn't available until Emacs 26, so we have to
                ;; manually track the arity of the functions.
                (`(nullary . ,f)
                 ;; a nullary function that returns a completion collection
                 (completion-table-with-cache
                  (lambda (_)
                    (cl-remove-if (apply-partially 'string= prefix) (funcall f)))))
                ((pred functionp)
                 ;; a completion table
                 collection)
                (_
                 ;; a static completion collection
                 collection))
              :exit-function (lambda (&rest _)
                               (when delete-suffix
                                 (delete-char delete-suffix))
                               (when (and realign-after ledger-post-auto-align)
                                 (ledger-post-align-postings (line-beginning-position) (line-end-position)))))))))

(defun ledger-trim-trailing-whitespace (str)
  (replace-regexp-in-string "[ \t]*$" "" str))

(defun ledger-fully-complete-xact ()
  "Completes a transaction if there is another matching payee in the buffer.

Interactively, if point is after a payee, complete the
transaction with the details from the last transaction to that
payee."
  (interactive)
  (let* ((name (ledger-trim-trailing-whitespace
                (buffer-substring
                 (save-excursion
                   (unless (eq (ledger-thing-at-point) 'transaction)
                     (user-error "Cannot fully complete xact here"))
                   (point))
                 (point))))
         (rest-of-name name)
         xacts)
    (save-excursion
      (when (eq 'transaction (ledger-thing-at-point))
        (delete-region (point) (+ (length name) (point)))
        ;; Search backward for a matching payee
        (when (re-search-backward
               (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.*"
                       (regexp-quote name) ".*\\)")
               nil t)
          (setq rest-of-name (match-string 3))
          ;; Start copying the postings
          (forward-line)
          (setq xacts (buffer-substring-no-properties (point) (ledger-navigate-end-of-xact))))))
    ;; Insert rest-of-name and the postings
    (save-excursion
      (insert rest-of-name ?\n)
      (insert xacts)
      (unless (looking-at-p "\n\n")
        (insert "\n")))
    (forward-line)
    (end-of-line)
    ;; Move to amount on first posting line
    (when (re-search-backward "\t\\| [ \t]" nil t)
      (goto-char (match-end 0)))))

(add-to-list 'completion-category-defaults '(ledger-date (styles . (substring))))

(provide 'ledger-complete)

;;; ledger-complete.el ends here
