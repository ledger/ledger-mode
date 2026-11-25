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
;; This file adds support for flymake to `ledger-mode'.  Enable it by calling
;; `ledger-flymake-enable' from a file-visiting ledger buffer.  To enable it
;; automatically, put this in your .emacs:
;;
;;     (add-hook 'ledger-mode-hook #'ledger-flymake-enable)

;;; Code:
(require 'cl-lib)
(require 'flymake)
(require 'ledger-exec)                  ; for `ledger-binary-path'
(require 'ledger-regex)
(require 'ledger-report)                ; for `ledger-master-file'

(defvar-local ledger--flymake-proc nil)

(defcustom ledger-flymake-be-pedantic nil
  "If non-nil, pass the --pedantic flag for ledger to the flymake backend.
If --pedantic is in your ledgerrc file, then --pedantic gets
passed regardless of the value."
  :type 'boolean
  :package-version '(ledger-mode . "4.0.0")
  :group 'ledger)

(defcustom ledger-flymake-be-explicit nil
  "If non-nil, pass the --explicit flag for ledger to the flymake backend.
If --explicit is in your ledgerrc file, then --explicit gets
passed regardless of the value."
  :type 'boolean
  :package-version '(ledger-mode . "4.0.0")
  :group 'ledger)

;; Based on the example from Flymake's info:
(defun ledger-flymake (report-fn &rest _args)
  "A Flymake backend for `ledger-mode'.

Flymake calls this with REPORT-FN as needed."
  (unless (executable-find ledger-binary-path)
    (error "Cannot find ledger"))
  ;; If a live process launched in an earlier check was found, that
  ;; process is killed.  When that process's sentinel eventually runs,
  ;; it will notice its obsoletion, since it have since reset
  ;; `ledger-flymake-proc' to a different value
  (when (process-live-p ledger--flymake-proc)
    (kill-process ledger--flymake-proc))
  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  (let* ((source (current-buffer))
         (file (or (ledger-master-file) (buffer-file-name))))
    (save-restriction
      (widen)
      ;; Reset the `ledger--flymake-proc' process to a new process
      ;; calling the ledger tool.
      (setq
       ledger--flymake-proc
       (make-process
        :name "ledger-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *ledger-flymake*")
        :command (cl-remove
                  nil
                  `(,ledger-binary-path "-f" ,file
                                        ,(when ledger-flymake-be-pedantic "--pedantic")
                                        ,(when ledger-flymake-be-explicit "--explicit")
                                        "balance"))
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
                       while (search-forward-regexp ledger-error-regex nil t)
                       for msg = (match-string 3)
                       for region = (flymake-diag-region
                                     source
                                     (string-to-number (match-string 2)))
                       when region
                       collect (flymake-make-diagnostic source
                                                        (car region)
                                                        (cdr region)
                                                        :error
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              ;; Cleanup the temporary buffer used to hold the
              ;; check's output.
              (kill-buffer (process-buffer proc))))))))))

;;;###autoload
(defun ledger-flymake-enable ()
  "Enable `flymake-mode' in `ledger-mode' buffers."
  ;; Add `ledger-flymake' to `flymake-diagnostic-functions' so that flymake can
  ;; work in ledger-mode:
  (add-hook 'flymake-diagnostic-functions #'ledger-flymake nil t)
  (flymake-mode))

(provide 'ledger-flymake)

;;; ledger-flymake.el ends here
