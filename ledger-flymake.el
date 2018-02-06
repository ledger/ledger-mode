;;; ledger-flymake.el --- A ledger Flymake backend  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 J. Alexander Branham (alex DOT branham AT gmail DOT com)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
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
;; Flymake is the built-in Emacs package to support on-the-fly syntax checking.
;; This file adds support for flymake to `ledger-mode'. Enable it by calling
;; `flymake-mode' from a file-visiting ledger buffer.

;;; Code:

(defvar-local ledger--flymake-proc nil)

;; Based on the example from Flymake's info:
(defun ledger-flymake (report-fn &rest _args)
  "A Flymake backend for `ledger-mode'.

Flymake calls this with REPORT-FN as needed."
  (unless (executable-find "ledger")
    (error "Cannot find ledger"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `ledger-flymake-proc' to a different value
  (when (process-live-p ledger--flymake-proc)
    (kill-process ledger--flymake-proc))
  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  (let ((source (current-buffer))
        (file (buffer-file-name)))
    (save-restriction
      (widen)
      ;; Reset the `ledger--flymake-proc' process to a new process
      ;; calling the ledger tool.
      (setq
       ledger--flymake-proc
       (make-process
        :name "ledger-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *ledger-flymake*")
        :command `("ledger" "-f" ,file "balance")
        :sentinel
        (lambda (proc _event)
          ;; Check that the process has indeed exited, as it might
          ;; be simply suspended.
          (when (eq 'exit (process-status proc))
            (unwind-protect
                ;; Only proceed if `proc' is the same as
                ;; `ledger--flymake-proc', which indicates that
                ;; `proc' is not an obsolete process.
                (if (with-current-buffer source (eq proc ledger--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      ;; Parse the output buffer for diagnostic's
                      ;; messages and locations, collect them in a list
                      ;; of objects, and call `report-fn'.
                      (cl-loop
                       while (search-forward-regexp
                              ;; This regex needs to match the whole error. We
                              ;; also need a capture group for the error message
                              ;; (that's group 1 here) and the line number
                              ;; (group 2).
                              (rx line-start "While parsing file \"" (one-or-more (not whitespace)) " line " (group-n 2 (one-or-more num)) ":\n"
                                  (zero-or-more line-start "While " (one-or-more not-newline) "\n" )
                                  (minimal-match (zero-or-more line-start (zero-or-more not-newline) "\n"))
                                  (group-n 1 "Error: " (one-or-more not-newline) "\n"))
                              nil t)
                       for msg = (match-string 1)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 2)))
                       for type = :error
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              (kill-buffer (process-buffer proc))))))))))

;; Automatically enable flymake in ledger-mode buffers when using Emacs 26+
(when (<= 26 emacs-major-version)
  ;; Add `ledger-flymake' to `flymake-diagnostic-functions' so that flymake can
  ;; work in ledger-mode:
  (add-hook 'flymake-diagnostic-functions 'ledger-flymake nil t)
  ;; Enable it!
  (add-hook 'ledger-mode-hook #'flymake-mode))

(provide 'ledger-flymake)

;;; ledger-flymake.el ends here
