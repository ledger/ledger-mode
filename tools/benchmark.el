;;; benchmark.el --- Microbenchmarks for ledger-mode hot paths  -*- lexical-binding: t; -*-

;; Copyright (c) 2007-2025 John Wiegley.  See LICENSE.md.

;;; Commentary:

;; A handful of small workloads exercising the operations users hit most often:
;; loading a buffer in `ledger-mode', fontifying it, navigating between
;; transactions, and running the regex matcher.  Each result is printed as a
;; single `name<TAB>seconds' line so that `tools/benchmark-check.sh' can diff
;; against `bench/baseline.txt'.

;;; Code:

(require 'benchmark)

(let* ((root (or (getenv "LEDGER_MODE_ROOT") default-directory)))
  (add-to-list 'load-path root))

(require 'ledger-mode)
(require 'ledger-fontify)
(require 'ledger-navigate)
(require 'ledger-regex)

(defvar ledger-mode/bench--demo
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "test/input/demo.ledger"
                       (or (getenv "LEDGER_MODE_ROOT") default-directory)))
    (buffer-string)))

(defvar ledger-mode/bench--big
  ;; Replicate the demo a few hundred times so the numbers are large enough to
  ;; be meaningful but small enough to finish in well under a second.
  (mapconcat #'identity (make-list 200 ledger-mode/bench--demo) "\n"))

(defmacro ledger-mode/bench (name iterations &rest body)
  (declare (indent defun))
  `(let ((res (benchmark-run ,iterations (progn ,@body))))
     (princ (format "%s\t%.4f\n" ,name (car res)))))

(defun ledger-mode/bench-run ()
  (ledger-mode/bench "load-mode" 50
                     (with-temp-buffer
                       (insert ledger-mode/bench--demo)
                       (delay-mode-hooks (ledger-mode))))

  (ledger-mode/bench "fontify-big" 5
                     (with-temp-buffer
                       (insert ledger-mode/bench--big)
                       (delay-mode-hooks (ledger-mode))
                       (font-lock-ensure)))

  (ledger-mode/bench "navigate-big" 20
                     (with-temp-buffer
                       (insert ledger-mode/bench--big)
                       (delay-mode-hooks (ledger-mode))
                       (goto-char (point-min))
                       (let ((prev -1))
                         (while (and (< prev (point)) (not (eobp)))
                           (setq prev (point))
                           (ledger-navigate-next-xact)))))

  (ledger-mode/bench "regex-iso-date" 5000
                     (string-match ledger-iso-date-regexp "2024/02/03")))

(let ((output (or (getenv "BENCH_OUT")
                  (expand-file-name "bench/current.txt"
                                    (or (getenv "LEDGER_MODE_ROOT")
                                        default-directory)))))
  (make-directory (file-name-directory output) t)
  (with-temp-file output
    (let ((standard-output (current-buffer)))
      (ledger-mode/bench-run))))

;;; benchmark.el ends here
