;;; coverage.el --- Run ert tests under undercover.el and emit LCOV  -*- lexical-binding: t; -*-

;; Copyright (c) 2007-2025 John Wiegley.  See LICENSE.md.

;;; Commentary:

;; Drives the existing ERT test suite while undercover instruments every
;; ledger-*.el source file.  An LCOV report is written to `coverage/lcov.info'
;; (or `COVERAGE_OUT' if set in the environment).
;;
;; To stay correct under parallel pre-commit hooks (where the byte-compile
;; check is busy creating `.elc' files in the project root), this script
;; rsync-copies the source tree into a private scratch directory and runs
;; under there.  undercover refuses to instrument any file that already has a
;; `.elc' next to it, so the isolation matters.
;;
;; Usage:
;;
;;     emacs -Q --batch --load tools/coverage.el

;;; Code:

(defvar ledger-mode/coverage-source
  (or (getenv "LEDGER_MODE_ROOT") default-directory))

(defvar ledger-mode/coverage-out
  (or (getenv "COVERAGE_OUT")
      (expand-file-name "coverage/lcov.info" ledger-mode/coverage-source)))

(make-directory (file-name-directory ledger-mode/coverage-out) t)

(defvar ledger-mode/coverage-root
  (let ((scratch (make-temp-file "ledger-mode-cov-" t)))
    ;; Copy *.el and the test/ tree into scratch; skip everything else
    ;; (build artefacts, .git, .elc files).
    (dolist (f (directory-files ledger-mode/coverage-source t "\\.el\\'"))
      (copy-file f (expand-file-name (file-name-nondirectory f) scratch)))
    (let ((dest-test (expand-file-name "test" scratch)))
      (make-directory dest-test t)
      (dolist (f (directory-files (expand-file-name "test" ledger-mode/coverage-source) t))
        (when (and (not (file-directory-p f))
                   (not (string-match-p "\\.elc\\'" f)))
          (copy-file f (expand-file-name (file-name-nondirectory f) dest-test)
                     t))) ; ok-if-already-exists
      (let ((src-input (expand-file-name "test/input" ledger-mode/coverage-source))
            (dst-input (expand-file-name "input" dest-test)))
        (make-directory dst-input t)
        (dolist (f (directory-files src-input t "[^.]"))
          (unless (file-directory-p f)
            (copy-file f (expand-file-name (file-name-nondirectory f) dst-input) t)))))
    scratch))

(setenv "UNDERCOVER_FORCE" "1")
(setenv "COVERAGE" "1")

(setq default-directory (file-name-as-directory ledger-mode/coverage-root))

(add-to-list 'load-path ledger-mode/coverage-root)
(add-to-list 'load-path (expand-file-name "test" ledger-mode/coverage-root))

(require 'undercover)
(setq undercover-force-coverage t)

(let ((default-directory (file-name-as-directory ledger-mode/coverage-root)))
  (undercover "ledger-*.el"
              (:report-format 'lcov)
              (:report-file (or (getenv "COVERAGE_OUT") "coverage/lcov.info"))
              (:merge-report nil)
              (:send-report nil)))

(load (expand-file-name "test/test-helper.el" ledger-mode/coverage-root) nil t)
(dolist (f (directory-files (expand-file-name "test" ledger-mode/coverage-root)
                            t "-test\\.el\\'"))
  (load f nil t))

(let ((stats (ert-run-tests-batch t)))
  (message "undercover: instrumented %d file(s)"
           (length (and (boundp 'undercover--files) undercover--files)))
  (when (fboundp 'undercover-safe-report)
    (undercover-safe-report))
  ;; The `:report-file' is relative to scratch's `default-directory'; copy
  ;; the result back to where the user expects it.
  (let ((scratch-out (expand-file-name "coverage/lcov.info"
                                       ledger-mode/coverage-root)))
    (when (file-exists-p scratch-out)
      (make-directory (file-name-directory ledger-mode/coverage-out) t)
      (copy-file scratch-out ledger-mode/coverage-out t)))
  ;; Clean up scratch.
  (delete-directory ledger-mode/coverage-root t)
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))

;;; coverage.el ends here
