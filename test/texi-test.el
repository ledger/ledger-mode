;;; texi-test.el --- ERT for ledger-texi  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2025 John Wiegley <johnw AT gnu DOT org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;;; Commentary:
;;  Regression tests for ledger-texi.

;;; Code:
(require 'test-helper)
(require 'ledger-texi)


(ert-deftest ledger-texi/expand-command-substitutes-LEDGER ()
  "$LEDGER is replaced with the configured binary path."
  (let ((ledger-binary-path "/tmp/ledger")
        (ledger-texi-normalization-args "--args-only --columns 80"))
    (let ((cmd (ledger-texi-expand-command "$LEDGER bal Assets" "/tmp/data.dat")))
      (should (string-match-p "/tmp/ledger" cmd))
      (should (string-match-p "/tmp/data.dat" cmd))
      (should (string-match-p "bal Assets" cmd))
      (should (string-match-p "--args-only --columns 80" cmd)))))

(ert-deftest ledger-texi/expand-command-no-LEDGER ()
  "Commands without $LEDGER are prefixed with the binary invocation."
  (let ((ledger-binary-path "/tmp/ledger")
        (ledger-texi-normalization-args "--columns 80"))
    (let ((cmd (ledger-texi-expand-command "bal Assets" "/tmp/data.dat")))
      (should (string-prefix-p "/tmp/ledger -f \"/tmp/data.dat\" --columns 80 " cmd))
      (should (string-suffix-p "bal Assets" cmd)))))

(ert-deftest ledger-texi/write-test-data ()
  "`ledger-texi-write-test-data' writes INPUT to a tmpdir file and returns the path."
  (let* ((name "ledger-texi-test-data.dat")
         (path (ledger-texi-write-test-data name "hello world")))
    (unwind-protect
        (progn
          (should (file-exists-p path))
          (with-temp-buffer
            (insert-file-contents path)
            (should (string-match-p "hello world" (buffer-string)))))
      (when (file-exists-p path) (delete-file path)))))

(ert-deftest ledger-texi/test-details ()
  "Parse a ledger-texi test buffer into (command input output)."
  (with-temp-buffer
    (insert "$LEDGER bal\n")
    (insert "<<<\n")
    (insert "2024/01/01 Acme\n")
    (insert "    Expenses:Foo  $10\n")
    (insert "    Assets:Cash\n")
    (insert ">>>1\n")
    (insert "          $-10  Assets:Cash\n")
    (insert "           $10  Expenses:Foo\n")
    (insert ">>>2\n")
    (insert "=== 0\n")
    (let ((details (ledger-texi-test-details)))
      (should (= 3 (length details)))
      (should (string-match-p "\\$LEDGER bal" (nth 0 details)))
      (should (string-match-p "Acme" (nth 1 details)))
      (should (string-match-p "Expenses:Foo" (nth 2 details))))))

(ert-deftest ledger-texi/write-test-creates-file ()
  "`ledger-texi-write-test' creates a .test file with the expected layout."
  (let ((tmp-dir (file-name-as-directory (make-temp-file "ledger-texi-" t))))
    (unwind-protect
        (progn
          (ledger-texi-write-test "demo" "$LEDGER bal" "input\n" "output\n" tmp-dir)
          (let ((p (expand-file-name "demo.test" tmp-dir)))
            (should (file-exists-p p))
            (with-temp-buffer
              (insert-file-contents p)
              (let ((s (buffer-string)))
                ;; The "$LEDGER " prefix is stripped from the command.
                (should-not (string-match-p "\\$LEDGER" s))
                (should (string-match-p "^bal" s))
                (should (string-match-p "<<<" s))
                (should (string-match-p "input" s))
                (should (string-match-p ">>>1" s))
                (should (string-match-p "output" s))
                (should (string-match-p ">>>2" s))
                (should (string-match-p "=== 0" s))))))
      (delete-directory tmp-dir t))))

(ert-deftest ledger-texi/write-test-strips-data-flag ()
  "The ` -f $foo' data-file flag (with a leading space) is stripped."
  (let ((tmp-dir (file-name-as-directory (make-temp-file "ledger-texi-" t))))
    (unwind-protect
        (progn
          (ledger-texi-write-test
           "with-data" "$LEDGER bal -f $sample" "in\n" "out\n" tmp-dir)
          (with-temp-buffer
            (insert-file-contents (expand-file-name "with-data.test" tmp-dir))
            (let ((s (buffer-string)))
              (should-not (string-match-p "\\$sample" s))
              (should-not (string-match-p "-f " s))
              (should (string-match-p "^bal" s)))))
      (delete-directory tmp-dir t))))

(ert-deftest ledger-texi/invoke-command-empty ()
  "`ledger-texi-invoke-command' beeps and returns nil for an empty result."
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_cmd _output buf)
               ;; Don't insert anything — simulate empty output.
               (with-current-buffer buf nil)))
            ((symbol-function 'ding) (lambda (&rest _) nil)))
    (should-not (ledger-texi-invoke-command "noop"))))

(ert-deftest ledger-texi/invoke-command-non-empty ()
  "`ledger-texi-invoke-command' returns the captured output as a string."
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_cmd _output buf)
               (with-current-buffer buf
                 (insert "captured output\n")))))
    (should (string= "captured output\n"
                     (ledger-texi-invoke-command "noop")))))

(ert-deftest ledger-texi/update-examples-substitutes-block ()
  "`ledger-texi-update-examples' replaces an @example block with command output."
  (cl-letf (((symbol-function 'ledger-texi-invoke-command)
             (lambda (_cmd) "FAKE LEDGER OUTPUT\n"))
            ((symbol-function 'ledger-texi-write-test)
             (lambda (&rest _) nil)))
    (let ((ledger-texi-sample-doc-path "/tmp/sample.dat"))
      (with-temp-buffer
        (insert "@c ex demo: $LEDGER bal Assets\n")
        (insert "@example\n")
        (insert "previous content\n")
        (insert "@end example\n")
        (ledger-texi-update-examples)
        (let ((s (buffer-string)))
          (should (string-match-p "FAKE LEDGER OUTPUT" s))
          (should-not (string-match-p "previous content" s)))))))

(ert-deftest ledger-texi/update-examples-smex ()
  "smex section uses smallexample wrappers."
  (cl-letf (((symbol-function 'ledger-texi-invoke-command)
             (lambda (_cmd) "small\n"))
            ((symbol-function 'ledger-texi-write-test)
             (lambda (&rest _) nil)))
    (let ((ledger-texi-sample-doc-path "/tmp/sample.dat"))
      (with-temp-buffer
        (insert "@c smex demo: $LEDGER bal\n")
        (ledger-texi-update-examples)
        (let ((s (buffer-string)))
          (should (string-match-p "@smallexample" s))
          (should (string-match-p "@end smallexample" s)))))))


(ert-deftest ledger-texi/update-test ()
  "`ledger-update-test' replaces the output region by calling the ledger binary."
  (cl-letf (((symbol-function 'call-process-region)
             (lambda (_beg _end _program &rest _args)
               ;; The destination is `t' (current buffer); our mock just inserts.
               (insert "MOCK OUTPUT\n")
               0)))
    (with-temp-buffer
      (insert "bal\n")
      (insert "<<<\n")
      (insert "2024/01/01 X\n")
      (insert "    Foo  $1\n")
      (insert "    Bar\n")
      (insert ">>>1\n")
      (insert "old output\n")
      (insert ">>>2\n")
      (insert "=== 0\n")
      (ledger-update-test)
      (let ((s (buffer-string)))
        (should (string-match-p "MOCK OUTPUT" s))
        (should-not (string-match-p "old output" s))))))


(ert-deftest ledger-texi/update-test-driver ()
  "`ledger-texi-update-test' wires together write-test/expand/invoke."
  (let* ((tmp-dir (file-name-as-directory (make-temp-file "ledger-texi-" t)))
         (file (expand-file-name "demo.test" tmp-dir))
         (calls (list)))
    (unwind-protect
        (cl-letf (((symbol-function 'ledger-texi-invoke-command)
                   (lambda (_cmd) "DRIVER OUTPUT\n"))
                  ((symbol-function 'ledger-texi-write-test)
                   (lambda (&rest args)
                     (push args calls))))
          (with-temp-buffer
            (set-visited-file-name file t)
            (insert "$LEDGER bal\n")
            (insert "<<<\n")
            (insert "2024/01/01 X\n")
            (insert "    Foo  $1\n")
            (insert "    Bar\n")
            (insert ">>>1\n")
            (insert "out\n")
            (insert ">>>2\n")
            (insert "=== 0\n")
            (ledger-texi-update-test)
            (set-buffer-modified-p nil))
          (should (= 1 (length calls)))
          (should (equal "demo" (nth 0 (car calls))))
          (should (equal "DRIVER OUTPUT\n" (nth 3 (car calls)))))
      (delete-directory tmp-dir t))))


(ert-deftest ledger-texi/update-examples-data-flag-substitution ()
  "When the command contains ` -f $label', `ledger-texi-update-examples'
substitutes the labelled data block."
  (cl-letf (((symbol-function 'ledger-texi-invoke-command)
             (lambda (_cmd) "FAKE\n"))
            ((symbol-function 'ledger-texi-write-test)
             (lambda (&rest _) nil))
            ((symbol-function 'ledger-texi-write-test-data)
             (lambda (name _input)
               (expand-file-name name temporary-file-directory))))
    (let ((ledger-texi-sample-doc-path "/tmp/sample.dat"))
      (with-temp-buffer
        (insert "@c data: mydata\n")
        (insert "@example\n")
        (insert "2024/01/01 Foo\n")
        (insert "    A  $1\n")
        (insert "    B\n")
        (insert "@end example\n")
        (insert "@c ex foo: $LEDGER bal -f $mydata\n")
        (insert "@example\n")
        (insert "previous content\n")
        (insert "@end example\n")
        (ledger-texi-update-examples)
        (let ((s (buffer-string)))
          (should (string-match-p "FAKE" s))
          (should-not (string-match-p "previous content" s)))))))


(provide 'texi-test)

;;; texi-test.el ends here
