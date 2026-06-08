;;; run-tests.el --- Load and run every ERT test in test/  -*- lexical-binding: t; -*-

;; Copyright (c) 2007-2025 John Wiegley.  See LICENSE.md.

;;; Commentary:

;; A small wrapper that mirrors what `make -C test' does, but in a form that
;; can be invoked from a Nix derivation without depending on GNU make.

;;; Code:

(define-advice ert-select-tests (:filter-return (selected-tests) randomize-order)
  "Randomize the order of ERT tests to avoid accidental state dependencies."
  (sort selected-tests
        (lambda (_ _) (zerop (random 2)))))

(let* ((root (or (getenv "LEDGER_MODE_ROOT") default-directory))
       (test-dir (expand-file-name "test" root)))
  (add-to-list 'load-path root)
  (add-to-list 'load-path test-dir)
  (load (expand-file-name "test-helper.el" test-dir) nil t)
  (dolist (f (directory-files test-dir t "-test\\.el\\'"))
    (load f nil t))
  (ert-run-tests-batch-and-exit t))

;;; run-tests.el ends here
