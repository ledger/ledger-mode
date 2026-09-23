;;; ledger-sort.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;;

;;; Code:
(require 'ledger-regex)
(require 'ledger-navigate)
(require 'ledger-xact)

(defun ledger-sort-find-start ()
  "Find the beginning of a sort region."
  (when (re-search-forward ";.*Ledger-mode:.*Start sort" nil t)
    (match-end 0)))

(defun ledger-sort-find-end ()
  "Find the end of a sort region."
  (when (re-search-forward ";.*Ledger-mode:.*End sort" nil t)
    (match-end 0)))

(defun ledger-sort-insert-start-mark ()
  "Insert a marker to start a sort region."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (ledger-sort-find-start)
      (delete-region (match-beginning 0) (match-end 0))))
  (beginning-of-line)
  (insert "\n; Ledger-mode: Start sort\n\n"))

(defun ledger-sort-insert-end-mark ()
  "Insert a marker to end a sort region."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (ledger-sort-find-end)
      (delete-region (match-beginning 0) (match-end 0))))
  (beginning-of-line)
  (insert "\n; Ledger-mode: End sort\n\n"))

(defconst ledger-sort--year-directive-regex
  "^\\(?:Y\\|year\\)\\s-+\\([0-9]+\\)\\s-*$"
  "Regex matching a `year NNNN' or `Y NNNN' directive at the start of a line.
The year number is captured in group 1.")

(defun ledger-sort--preceding-year ()
  "Return the year from the most recent `year NNNN' or `Y NNNN' directive.
Searches backward from the start of the current line, ignoring any
restriction so that directives above a narrowed sort region are still
consulted.  Returns a number, or nil if no such directive exists."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (when (re-search-backward ledger-sort--year-directive-regex nil t)
        (string-to-number (match-string 1))))))

(defun ledger-sort-startkey ()
  "Return a numeric sort key based on the date of the xact beginning at point.
Dates with a full four-digit year are parsed directly.  Short dates of the
form M/D or MM/DD are interpreted relative to the most recent `year NNNN'
directive preceding the current transaction, falling back to the current
calendar year if no such directive exists."
  ;; Can use `time-convert' to return an integer instead of a floating-point
  ;; number, starting in Emacs 27.
  (float-time
   (cond
    ((looking-at ledger-iso-date-regexp)
     (ledger-parse-iso-date (match-string 0)))
    ((looking-at "\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\(?:[^-/0-9]\\|$\\)")
     (let ((month (string-to-number (match-string 1)))
           (day   (string-to-number (match-string 2)))
           (year  (or (ledger-sort--preceding-year)
                      (nth 5 (decode-time)))))
       (encode-time 0 0 0 day month year))))))

(defun ledger-sort-region (beg end)
  "Sort the region from BEG to END in chronological order."
  (interactive "r") ;; load beg and end from point and mark
  ;; automagically
  (let* ((bounds (ledger-navigate-find-xact-extents (point)))
         (point-delta (- (point) (car bounds)))
         (target-xact (buffer-substring (car bounds) (cadr bounds)))
         (inhibit-modification-hooks t))
    (save-excursion
      (save-restriction
        (goto-char beg)
        ;; make sure beg of region is at the beginning of a line
        (beginning-of-line)
        ;; make sure point is at the beginning of a xact
        (unless (looking-at-p ledger-payee-any-status-regex)
          (ledger-navigate-next-xact))
        (setq beg (point))
        (goto-char end)
        (ledger-navigate-next-xact)
        ;; make sure end of region is at the beginning of next record
        ;; after the region
        (setq end (point))
        (narrow-to-region beg end)
        (goto-char beg)

        (let ((inhibit-field-text-motion t))
          (sort-subr
           nil
           #'ledger-navigate-next-xact
           #'ledger-navigate-end-of-xact
           #'ledger-sort-startkey))))

    (goto-char (point-min))
    (search-forward target-xact)
    (goto-char (+ (match-beginning 0) point-delta))))

(defun ledger-sort-buffer ()
  "Sort the entire buffer."
  (interactive)
  (let (sort-start sort-end)
    (save-excursion
      (goto-char (point-min))
      (setq sort-start (ledger-sort-find-start)
            sort-end (ledger-sort-find-end)))
    (ledger-sort-region (or sort-start (point-min))
                        (or sort-end (point-max)))))

(provide 'ledger-sort)

;;; ledger-sort.el ends here
