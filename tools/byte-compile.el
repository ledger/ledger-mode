;;; byte-compile.el --- Byte-compile every source file with warnings as errors  -*- lexical-binding: t; -*-

;; Copyright (c) 2007-2025 John Wiegley.  See LICENSE.md.

;;; Commentary:

;; Wrapped by `nix flake check' and `lefthook'.  Compiles every top-level
;; *.el file with `byte-compile-error-on-warn' set so that any warning fails
;; the build.

;;; Code:

(setq byte-compile-error-on-warn t)
(setq byte-compile-warnings t)

(let* ((root (or (getenv "LEDGER_MODE_ROOT") default-directory))
       (args (seq-remove (lambda (a) (string= a "--")) command-line-args-left))
       (files (or args
                  (directory-files root t "\\`ledger-.*\\.el\\'"))))
  (setq command-line-args-left nil)
  (add-to-list 'load-path root)
  (let ((failed 0))
    (dolist (f files)
      (unless (byte-compile-file f)
        (setq failed (1+ failed))))
    (kill-emacs (if (zerop failed) 0 1))))

;;; byte-compile.el ends here
