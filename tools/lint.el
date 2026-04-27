;;; lint.el --- Run package-lint and checkdoc on every ledger-mode source file  -*- lexical-binding: t; -*-

;; Copyright (c) 2007-2025 John Wiegley.  See LICENSE.md.

;;; Commentary:

;; Invoked from the Nix `lint' check and the lefthook pre-commit hook:
;;
;;     emacs -Q --batch --load tools/lint.el [files...]
;;
;; If no files are passed it lints every top-level *.el file in the project.
;;
;; By default both `package-lint' and `checkdoc' findings are printed but
;; informational — the build succeeds as long as both tools run.  This matches
;; the long-standing convention in this repository's CI, where the existing
;; corpus has known checkdoc and package-lint debt that is being worked off
;; gradually.  Set `LEDGER_MODE_STRICT_LINT=1' to make either fatal.

;;; Code:

(require 'package-lint nil t)
(require 'checkdoc)

(defvar ledger-mode/lint--package-lint-problems 0)
(defvar ledger-mode/lint--checkdoc-problems 0)

(defun ledger-mode/lint--root ()
  (or (getenv "LEDGER_MODE_ROOT") default-directory))

(defun ledger-mode/lint--collect-files ()
  (or (seq-remove (lambda (a) (string= a "--")) command-line-args-left)
      (let ((default-directory (ledger-mode/lint--root)))
        (sort
         (seq-remove
          (lambda (f)
            (string-match-p "/\\(test\\|tools\\|bench\\|\\.git\\|result\\)/" f))
          (directory-files-recursively default-directory "\\`ledger-.*\\.el\\'"))
         #'string-lessp))))

(defun ledger-mode/lint--package-lint (files)
  ;; ledger-mode is a multi-file package: only the entry file (ledger-mode.el)
  ;; carries the MELPA recipe metadata.  package-lint treats each file as its
  ;; own package by default, so restrict it to the main file only.
  (if (not (fboundp 'package-lint-buffer))
      (message "package-lint not available; skipping")
    (dolist (file files)
      (when (string= (file-name-nondirectory file) "ledger-mode.el")
        (with-temp-buffer
          (insert-file-contents file)
          (emacs-lisp-mode)
          (setq buffer-file-name file)
          (let ((problems (package-lint-buffer)))
            (when problems
              (setq ledger-mode/lint--package-lint-problems
                    (+ ledger-mode/lint--package-lint-problems (length problems)))
              (dolist (p problems)
                (let ((line (nth 0 p)) (col (nth 1 p))
                      (severity (nth 2 p)) (msg (nth 3 p)))
                  (message "%s:%d:%d: %s: %s" file line col severity msg))))))))))

(defun ledger-mode/lint--checkdoc (files)
  (dolist (file files)
    (let ((checkdoc-create-error-function
           (lambda (text start _end &optional _unfixable)
             (setq ledger-mode/lint--checkdoc-problems
                   (1+ ledger-mode/lint--checkdoc-problems))
             (let ((line (or (and start
                                  (with-current-buffer (or (find-buffer-visiting file)
                                                           (current-buffer))
                                    (line-number-at-pos start)))
                             0)))
               (message "%s:%d: checkdoc: %s" file line text)
               nil))))
      (ignore-errors (checkdoc-file file)))))

(defun ledger-mode/lint-main ()
  ;; package-lint problems are fatal; checkdoc remains informational because
  ;; the existing corpus has long-standing checkdoc debt.  Set
  ;; `LEDGER_MODE_STRICT_CHECKDOC=1' to make checkdoc fatal too.
  (let ((files (ledger-mode/lint--collect-files))
        (strict-checkdoc (getenv "LEDGER_MODE_STRICT_CHECKDOC")))
    (message "Linting %d file(s)" (length files))
    (ledger-mode/lint--package-lint files)
    (ledger-mode/lint--checkdoc files)
    (message "package-lint problems: %d, checkdoc problems: %d (strict-checkdoc=%s)"
             ledger-mode/lint--package-lint-problems
             ledger-mode/lint--checkdoc-problems
             (if strict-checkdoc "on" "off"))
    (kill-emacs
     (cond
      ((> ledger-mode/lint--package-lint-problems 0) 1)
      ((and strict-checkdoc
            (> ledger-mode/lint--checkdoc-problems 0)) 1)
      (t 0)))))

(ledger-mode/lint-main)

;;; lint.el ends here
