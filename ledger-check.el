;;; ledger-check.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Craig Earls (enderw88 AT gmail DOT com)

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
;;  Provide secial mode to correct errors in ledger when running with --strict and --explicit
;;
;; Adapted to ledger mode by Craig Earls <enderw88 at gmail dot com>

;;; Code:

(require 'easymenu)
(require 'ledger-navigate)
(require 'ledger-report)


(defvar ledger-check-buffer-name "*Ledger Check*")
(defvar-local ledger-check--original-window-configuration nil)
(defvar-local ledger-check--source-buffer nil)




(defvar ledger-check-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ledger-report-visit-source)
    (define-key map (kbd "r") #'ledger-check-redo)
    (define-key map (kbd "q") #'ledger-check-quit)
    map)
  "Keymap for `ledger-check-mode'.")

(easy-menu-define ledger-check-mode-menu ledger-check-mode-map
  "Ledger check menu."
  '("Check"
    ["Re-run Check" ledger-check-redo]
    "---"
    ["Visit Source" ledger-report-visit-source]
    "---"
    ["Quit" ledger-check-quit]
    ))

(define-derived-mode ledger-check-mode special-mode "Ledger-Check"
  "A mode for viewing ledger errors and warnings.")

(defun ledger-do-check ()
  "Run a check command and put the output in the current buffer."
  (with-silent-modifications
    (erase-buffer)

    (let ((cbuf (current-buffer)))
      (with-current-buffer ledger-check--source-buffer
        ;;  ledger balance command will just return empty if you give it
        ;;  an account name that doesn't exist.  I will assume that no
        ;;  one will ever have an account named "e342asd2131".  If
        ;;  someones does, this will probably still work for them.
        ;;  I should only highlight error and warning lines.
        (call-process-region (point-min) (point-max)
                             "ledger"
                             nil cbuf t
                             "bal" "e342asd2131" "--strict" "--explicit" "--file=-")))

    ;; format check report to make it navigate the file

    (goto-char (point-min))
    (while (re-search-forward "^.*: \".*\", line \\([0-9]+\\)" nil t)
      (let* ((line (string-to-number (match-string 1)))
             (source-marker
              (with-current-buffer ledger-check--source-buffer
                (save-excursion
                  (save-restriction
                    (widen)
                    (ledger-navigate-to-line line)
                    (point-marker))))))
        (set-text-properties (line-beginning-position) (line-end-position)
                             (list 'ledger-source source-marker
                                   'face 'ledger-font-report-clickable-face))
        (end-of-line)))
    (when (= (buffer-size) 0)
      (insert "No errors or warnings reported.\n"))))

(defun ledger-check-goto ()
  "Goto the ledger check buffer."
  (interactive)
  (let ((rbuf (get-buffer ledger-check-buffer-name)))
    (unless rbuf
      (user-error "There is no ledger check buffer"))
    (pop-to-buffer rbuf
                   '(display-buffer-below-selected
                     (window-height . shrink-window-if-larger-than-buffer)))))

(defun ledger-check-redo ()
  "Re-run the check command for the current output buffer."
  (interactive)
  (ledger-do-check)
  (goto-char (point-min))
  (message (substitute-command-keys "\\[ledger-check-quit] to quit; \\[ledger-check-redo] to redo")))

(defun ledger-check-quit ()
  "Quit the ledger check buffer."
  (interactive)
  (ledger-check-goto)
  (set-window-configuration ledger-check--original-window-configuration)
  (kill-buffer ledger-check-buffer-name))

;; FIXME: This is an awful lot of faff, couldn't we just use the report logic
;; and maybe define a custom "check" report? It would work better in most ways,
;; just need to add any missing features like window config restore (and maybe
;; it should use markers).
(defun ledger-check-buffer (&optional interactive)
  "Check the current buffer for errors.

Runs ledger with --explicit and --strict report errors and assist
with fixing them.

The output buffer will be in `ledger-check-mode', which defines
commands for navigating the buffer to the errors found, etc.

When INTERACTIVE is non-nil (i.e., when called interactively),
prompt to save if the current buffer is modified."
  (interactive "p")
  (when (and interactive
             (buffer-modified-p)
             (y-or-n-p "Buffer modified, save it? "))
    (save-buffer))
  (let ((source-buffer (current-buffer))
        (cbuf (get-buffer ledger-check-buffer-name))
        (wcfg (current-window-configuration)))
    (if cbuf
        (kill-buffer cbuf))
    (with-current-buffer (get-buffer-create ledger-check-buffer-name)
      (ledger-check-mode)
      (setq ledger-check--source-buffer source-buffer
            ledger-check--original-window-configuration wcfg)
      (pop-to-buffer (current-buffer)
                     '(display-buffer-below-selected
                       (window-height . shrink-window-if-larger-than-buffer)))
      (ledger-check-redo))))


(provide 'ledger-check)

;;; ledger-check.el ends here
