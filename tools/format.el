;;; format.el --- Reformat ledger-mode source files in place  -*- lexical-binding: t; -*-

;; Copyright (c) 2007-2025 John Wiegley.  See LICENSE.md.

;;; Commentary:

;; Emacs Lisp has no equivalent of `gofmt' or `rustfmt' that can be run on a
;; mature codebase without rewriting nearly every line.  This script applies
;; the conservative set of transformations that everyone agrees on:
;;
;;   * trailing whitespace stripped from every line
;;   * exactly one trailing newline at the end of every file
;;   * UTF-8 LF line endings
;;
;; Structural reindentation and tab/space conversion are intentionally skipped
;; because (a) the editor that opens these files (with the package loaded so
;; macro indent declarations are honoured) is the source of truth, and (b) the
;; ert tests embed tabs in string literals as test data.
;;
;; Usage:
;;
;;     emacs -Q --batch --load tools/format.el [files...]

;;; Code:

(defun ledger-mode/format--root ()
  (or (getenv "LEDGER_MODE_ROOT") default-directory))

(defun ledger-mode/format--collect-files ()
  (let ((args (seq-remove (lambda (a) (string= a "--")) command-line-args-left)))
    (setq command-line-args-left nil)
    (or args
        (let ((default-directory (ledger-mode/format--root)))
          (sort
           (seq-remove
            (lambda (f) (string-match-p "/\\(\\.git\\|result\\|bench\\)/" f))
            (directory-files-recursively default-directory "\\.el\\'"))
           #'string-lessp)))))

(defun ledger-mode/format--buffer ()
  (delete-trailing-whitespace)
  (unless (zerop (point-max))
    (goto-char (point-max))
    (skip-chars-backward "\n")
    (delete-region (point) (point-max))
    (insert "\n")))

(defun ledger-mode/format-main ()
  (let ((files (ledger-mode/format--collect-files))
        (errors 0))
    (dolist (file files)
      (condition-case err
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8-unix))
              (insert-file-contents file))
            (setq buffer-file-name file
                  buffer-file-coding-system 'utf-8-unix)
            (ledger-mode/format--buffer)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) file nil 'silent)))
        (error
         (setq errors (1+ errors))
         (message "Error formatting %s: %s" file (error-message-string err)))))
    (kill-emacs (if (zerop errors) 0 1))))

(ledger-mode/format-main)

;;; format.el ends here
