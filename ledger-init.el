;;; ledger-init.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;; Determine the ledger environment

(require 'ledger-regex)

;;; Code:

(defcustom ledger-init-file-name "~/.ledgerrc"
  "Location of the ledger initialization file.  nil if you don't have one."
  :type '(choice (const :tag "Do not read ledger initialization file" nil)
                 file)
  :group 'ledger-exec)

(defvar ledger-environment-alist nil
  "Variable to hold details about ledger-mode's environment.

Adding the dotted pair (\"decimal-comma\" . t) will tell ledger
to treat commas as decimal separator.

This variable is automatically populated by
`ledger-init-load-init-file', which is called in the body of
`ledger-mode'.")

(defconst ledger-iso-date-format "%Y-%m-%d"
  "The format for ISO 8601 dates.")

(defcustom ledger-default-date-format "%Y/%m/%d"
  "The date format that ledger uses throughout.
Set this to the value of `ledger-iso-date-format' if you prefer
ISO 8601 dates."
  :type 'string
  :package-version '(ledger-mode . "4.0.0")
  :group 'ledger)

(defun ledger-format-date (&optional date format)
  "Format DATE according to the current preferred date format.
Returns the current date if DATE is nil or not supplied.

If FORMAT is provided, use that as the date format.  Otherwise,
use the --input-date-format specified in `ledger-init-file-name',
or if none, use `ledger-default-date-format'."
  (format-time-string
   (or format
       (cdr (assoc "input-date-format" ledger-environment-alist))
       ledger-default-date-format)
   date))


(defun ledger-init-parse-initialization (buffer)
  "Parse the .ledgerrc file in BUFFER."
  (with-current-buffer buffer
    (let (environment-alist)
      (goto-char (point-min))
      (while (re-search-forward ledger-init-string-regex nil t)
        (let ((matchb (match-beginning 0)) ;; save the match data, string-match stamp on it
              (matche (match-end 0)))
          (end-of-line)
          (push (cons (let ((flag (buffer-substring-no-properties (+ 2 matchb) matche)))
                        (if (string-match "[ \t\n\r]+\\'" flag)
                            (replace-match "" t t flag)
                          flag))
                      (let ((value (buffer-substring-no-properties matche (point))))
                        (if (> (length value) 0)
                            value
                          t)))
                environment-alist)))
      (nreverse environment-alist))))

(defun ledger-init-load-init-file ()
  "Load and parse the .ledgerrc file into `ledger-environment-alist'."
  (interactive)
  (when (and ledger-init-file-name
             (file-readable-p ledger-init-file-name))
    (with-temp-buffer
      (insert-file-contents ledger-init-file-name)
      (setq ledger-environment-alist
            (ledger-init-parse-initialization (current-buffer))))))

(provide 'ledger-init)

;;; ledger-init.el ends here
