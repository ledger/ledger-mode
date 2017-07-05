;;; ledger-fontify.el --- Provide custom fontification for ledger-mode  -*- lexical-binding: t; -*-


;; Copyright (C) 2014 Craig P. Earls (enderw88 at gmail dot com)

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
;;  Font-lock-mode doesn't handle multiline syntax very well.  This
;;  code provides font lock that is sensitive to overall transaction
;;  states


;;; Code:

(require 'ledger-navigate)
(require 'ledger-regex)
(require 'ledger-state)

(defcustom ledger-fontify-xact-state-overrides nil
  "If t the highlight entire xact with state."
  :type 'boolean
  :group 'ledger)

(defun ledger-fontify-buffer-part (beg end &rest _ignored)
  "Fontify buffer from BEG to END.
This is for use as a `font-lock-fontify-region-function'."
  (save-excursion
    (unless beg (setq beg (point-min)))
    (unless end (setq end (point-max)))
    (goto-char beg)
    (beginning-of-line)
    (while (< (point) end)
      (cond ((or (looking-at ledger-xact-start-regex)
                 (looking-at "^[=~][[:blank:]]")
                 (looking-at ledger-posting-regex)
                 (looking-at ledger-recurring-line-regexp)
                 (looking-at ledger-directive-start-regex))
             (let ((extents (ledger-navigate-find-element-extents (point))))
               (save-excursion
                 (font-lock-fontify-keywords-region (car extents) (cadr extents))))))
      (ledger-navigate-next-xact-or-directive))))


(provide 'ledger-fontify)

;;; ledger-fontify.el ends here
