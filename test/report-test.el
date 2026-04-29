;;; report-test.el --- ERT for ledger-report  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2025 John Wiegley <johnw AT gnu DOT org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;;; Commentary:
;;  Regression tests for ledger-report.

;;; Code:
(require 'test-helper)
(require 'ledger-report)


;;; Pure helpers ----------------------------------------------------------

(ert-deftest ledger-report/normalize-month ()
  :tags '(report)
  (should (equal (ledger-report--normalize-month '(2018 . 10)) '(2018 . 10)))
  (should (equal (ledger-report--normalize-month '(2018 . 0)) '(2017 . 12)))
  (should (equal (ledger-report--normalize-month '(2018 . -2)) '(2017 . 10)))
  (should (equal (ledger-report--normalize-month '(2018 . 13)) '(2019 . 1)))
  (should (equal (ledger-report--normalize-month '(2018 . 12)) '(2018 . 12)))
  (should (equal (ledger-report--normalize-month '(2018 . -12)) '(2016 . 12))))

(ert-deftest ledger-report/shift-month ()
  :tags '(report)
  (should (equal (ledger-report--shift-month '(2018 . 10) 2) '(2018 . 12)))
  (should (equal (ledger-report--shift-month '(2018 . 10) 3) '(2019 . 1)))
  (should (equal (ledger-report--shift-month '(2018 . 1) -1) '(2017 . 12)))
  (should (equal (ledger-report--shift-month '(2018 . 1) -13) '(2016 . 12))))

(ert-deftest ledger-report/current-month ()
  (let ((m (ledger-report--current-month)))
    (should (consp m))
    (should (integerp (car m)))
    (should (and (>= (cdr m) 1) (<= (cdr m) 12)))))

(ert-deftest ledger-report/binary-format-specifier ()
  (let ((ledger-binary-path "/usr/local/bin/ledger"))
    (let ((spec (ledger-report-binary-format-specifier)))
      (should (= 2 (length spec)))
      (should (string-match-p "/usr/local/bin/ledger" (car spec)))
      (should (string= (cadr spec) "[[ledger-mode-flags]]")))))

(ert-deftest ledger-report/master-file-no-override ()
  (with-temp-buffer
    (let ((ledger-master-file nil))
      (setq buffer-file-name "/tmp/foo.ledger")
      (should (string= "/tmp/foo.ledger" (ledger-master-file)))
      (should (string= "/tmp/foo.ledger" (ledger-report-ledger-file-format-specifier))))))

(ert-deftest ledger-report/master-file-overridden ()
  (with-temp-buffer
    (let ((ledger-master-file "/tmp/master.ledger"))
      (should (string= (expand-file-name "/tmp/master.ledger") (ledger-master-file))))))


;;; Reports list management ----------------------------------------------

(ert-deftest ledger-report/name-exists ()
  (let ((ledger-reports '(("Foo" "ledger bal Foo") ("Bar" "ledger reg Bar"))))
    (should (equal "Foo" (ledger-report-name-exists "Foo")))
    (should-not (ledger-report-name-exists "Missing"))
    (should-not (ledger-report-name-exists ""))))

(ert-deftest ledger-report/reports-add ()
  (let ((ledger-reports nil))
    (ledger-reports-add "X" "ledger bal")
    (should (equal '(("X" "ledger bal")) ledger-reports))
    (ledger-reports-add "Y" "ledger reg")
    (should (= 2 (length ledger-reports)))))


;;; Format specifiers ----------------------------------------------------

(ert-deftest ledger-report/format-specifier-tagname ()
  (cl-letf (((symbol-function 'ledger-read-string-with-default)
             (lambda (_prompt _default) "tag-X")))
    (should (string= "tag-X" (ledger-report-tagname-format-specifier)))))

(ert-deftest ledger-report/format-specifier-tagvalue ()
  (cl-letf (((symbol-function 'ledger-read-string-with-default)
             (lambda (_prompt _default) "tag-V")))
    (should (string= "tag-V" (ledger-report-tagvalue-format-specifier)))))

(ert-deftest ledger-report/format-specifier-payee ()
  (cl-letf (((symbol-function 'ledger-read-payee-with-prompt)
             (lambda (_prompt) "Payee Name")))
    (should (string= "Payee Name" (ledger-report-payee-format-specifier)))))

(ert-deftest ledger-report/format-specifier-account ()
  (cl-letf (((symbol-function 'ledger-read-account-with-prompt)
             (lambda (_prompt) "Assets:Bank")))
    (should (string= "Assets:Bank" (ledger-report-account-format-specifier)))))

(ert-deftest ledger-report/month-format-specifier-default ()
  ;; The function does `(with-current-buffer (or ledger-report-buffer-name …))',
  ;; so the report buffer must exist.
  (let ((buf (get-buffer-create ledger-report-buffer-name)))
    (unwind-protect
        (let ((ledger-report-current-month '(2024 . 4)))
          (should (string= "2024-4" (ledger-report-month-format-specifier))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest ledger-report/expand-format-specifiers ()
  ;; Substitute %(month) and %(foo).  Needs both a ledger-buf and the report
  ;; buffer because month-format-specifier-default switches there.
  (let ((report-buf (get-buffer-create ledger-report-buffer-name))
        (src-buf (current-buffer)))
    (unwind-protect
        (let ((ledger-report-current-month '(2024 . 4))
              (ledger-report-ledger-buf src-buf)
              (ledger-report-format-specifiers
               '(("month" . ledger-report-month-format-specifier)
                 ("foo" . (lambda () "QUOTED ME")))))
          (let ((result (ledger-report-expand-format-specifiers
                         "ledger -p %(month) reg %(foo)")))
            (should (string-match-p "ledger -p 2024-4 reg" result))
            ;; shell-quote-argument escapes the space, so the literal output
            ;; contains "QUOTED\ ME".
            (should (string-match-p "QUOTED" result))
            (should (string-match-p "ME" result))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer report-buf)))))

(ert-deftest ledger-report/expand-format-specifiers-list ()
  ;; A list-returning specifier is space-joined, not shell-quoted.
  (let ((ledger-report-ledger-buf (current-buffer))
        (ledger-report-format-specifiers
         '(("multi" . (lambda () (list "a" "b" "c"))))))
    (let ((result (ledger-report-expand-format-specifiers
                   "ledger %(multi) reg")))
      (should (string-match-p "ledger a b c reg" result)))))


;;; Header line + extra args ---------------------------------------------

(ert-deftest ledger-report/cmd-needs-links-p ()
  (let ((ledger-report-links-in-register t))
    (should (ledger-report--cmd-needs-links-p "reg --weekly"))
    (should (ledger-report--cmd-needs-links-p "register"))
    (should-not (ledger-report--cmd-needs-links-p "bal Assets"))
    (should-not (ledger-report--cmd-needs-links-p "reg --subtotal")))
  (let ((ledger-report-links-in-register nil))
    (should-not (ledger-report--cmd-needs-links-p "reg"))))

(ert-deftest ledger-report/compute-extra-args ()
  (let ((ledger-report-links-in-register t)
        (ledger-report-auto-width nil)
        (ledger-report-use-native-highlighting nil)
        (ledger-report-use-strict t))
    (let ((args (ledger-report--compute-extra-args "reg")))
      (should (member "--strict" args))
      (should (member "--prepend-format=%(filename):%(beg_line):" args)))))

(ert-deftest ledger-report/compute-extra-args-bal-no-links ()
  (let ((ledger-report-links-in-register t)
        (ledger-report-auto-width nil)
        (ledger-report-use-strict nil)
        (ledger-report-use-native-highlighting nil))
    (let ((args (ledger-report--compute-extra-args "bal")))
      (should-not (member "--prepend-format=%(filename):%(beg_line):" args)))))


;;; Save / read-name / read-command --------------------------------------

(ert-deftest ledger-report/read-name-via-completing-read ()
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "PickedName")))
    (let ((ledger-reports '(("PickedName" "ledger bal"))))
      (should (string= "PickedName" (ledger-report-read-name))))))

(ert-deftest ledger-report/read-new-name-rejects-empty ()
  (let ((tries '("" "" "Finally")))
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _args) (pop tries))))
      (should (string= "Finally" (ledger-report-read-new-name))))))

(ert-deftest ledger-report/read-command-default ()
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (_prompt initial &rest _rest) initial)))
    (should (string= "ledger " (ledger-report-read-command nil)))
    (should (string= "ledger bal X"
                     (ledger-report-read-command "ledger bal X")))))


;;; Header function ------------------------------------------------------

(ert-deftest ledger-report/header-function ()
  (with-temp-buffer
    (rename-buffer "ledger-buf-x" t)
    (let ((src (current-buffer)))
      (with-temp-buffer
        (rename-buffer "ledger-report-x" t)
        (let ((ledger-report-name "Foo")
              (ledger-report-cmd "ledger bal")
              (ledger-report-ledger-buf src))
          (let ((s (ledger-report--header-function)))
            (should (string-match-p "Ledger Report: Foo" s))
            (should (string-match-p "ledger-buf-x" s))
            (should (string-match-p "ledger bal" s))))))))


;;; Quit / goto / edit ---------------------------------------------------

(ert-deftest ledger-report/goto-no-buffer-errors ()
  (when (get-buffer ledger-report-buffer-name)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer ledger-report-buffer-name)))
  (should-error (ledger-report-goto)))

(ert-deftest ledger-report/quit-no-buffer-errors ()
  (when (get-buffer ledger-report-buffer-name)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer ledger-report-buffer-name)))
  (should-error (ledger-report-quit)))

(ert-deftest ledger-report/edit-report ()
  ;; ledger-report-edit-report reads a new command and re-runs.
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _) "ledger reg --weekly"))
            ((symbol-function 'ledger-report-redo)
             (lambda (&rest _) (setq ledger-report-cmd ledger-report-cmd))))
    (let ((ledger-report-cmd "ledger bal"))
      (ledger-report-edit-report)
      (should (string= "ledger reg --weekly" ledger-report-cmd)))))

(ert-deftest ledger-report/edit-reports ()
  ;; Just verifies it dispatches to customize-variable.
  (let ((called nil))
    (cl-letf (((symbol-function 'customize-variable)
               (lambda (var) (setq called var))))
      (ledger-report-edit-reports)
      (should (eq called 'ledger-reports)))))


;;; Reverse / change-month ----------------------------------------------

(ert-deftest ledger-report/reverse-lines ()
  ;; Without header line: skip first paragraph + 1 line.
  (with-temp-buffer
    (let ((ledger-report-use-header-line nil))
      (insert "Header\n\n")
      (insert "line A\n")
      (insert "line B\n")
      (ledger-report-reverse-lines)
      ;; The first two lines (Header + blank + first content line) form the
      ;; paragraph which is preserved; the rest is reversed.
      (let ((s (buffer-string)))
        (should (string-match-p "Header" s))))))

(ert-deftest ledger-report/reverse-report-toggles-flag ()
  (with-temp-buffer
    (let ((ledger-report-use-header-line t)
          (ledger-report-is-reversed nil))
      (insert "row 1\nrow 2\nrow 3\n")
      (ledger-report-reverse-report)
      (should (eq t ledger-report-is-reversed))
      (ledger-report-reverse-report)
      (should-not ledger-report-is-reversed))))


;;; Saving named reports ------------------------------------------------

(ert-deftest ledger-report/save-asks-for-name ()
  ;; When the report name is empty, save prompts for one and stores it in
  ;; `ledger-report-name'.  Adding to `ledger-reports' is the responsibility
  ;; of `ledger-report-cmd', not of save itself when the name is brand new.
  (cl-letf (((symbol-function 'ledger-report-goto) (lambda () nil))
            ((symbol-function 'read-from-minibuffer)
             (lambda (&rest _) "MyReport"))
            ((symbol-function 'customize-save-variable)
             (lambda (&rest _) nil)))
    (let ((ledger-reports nil)
          (ledger-report-name "")
          (ledger-report-cmd "ledger bal"))
      (ledger-report-save)
      (should (string= "MyReport" ledger-report-name)))))

(ert-deftest ledger-report/save-overwrites-existing ()
  (cl-letf (((symbol-function 'ledger-report-goto) (lambda () nil))
            ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
            ((symbol-function 'customize-save-variable)
             (lambda (&rest _) nil)))
    (let ((ledger-reports '(("MyReport" "ledger bal old")))
          (ledger-report-name "MyReport")
          (ledger-report-cmd "ledger bal new"))
      (ledger-report-save)
      (should (string= "ledger bal new"
                       (cadr (assoc "MyReport" ledger-reports)))))))

(ert-deftest ledger-report/save-overwrite-identical-noop ()
  (let ((message-log-max nil)
        (logged nil))
    (cl-letf (((symbol-function 'ledger-report-goto) (lambda () nil))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'message)
               (lambda (msg &rest _) (setq logged msg)))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) nil)))
      (let ((ledger-reports '(("MyReport" "ledger bal")))
            (ledger-report-name "MyReport")
            (ledger-report-cmd "ledger bal"))
        (ledger-report-save)
        (should (string-match-p "Nothing to save" logged))))))

(ert-deftest ledger-report/save-decline-rename ()
  (let ((prompts (list "" "AnotherName")))
    (cl-letf (((symbol-function 'ledger-report-goto) (lambda () nil))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) (pop prompts)))
              ((symbol-function 'customize-save-variable)
               (lambda (&rest _) nil)))
      (let ((ledger-reports '(("MyReport" "ledger bal")))
            (ledger-report-name "MyReport")
            (ledger-report-cmd "ledger bal new"))
        (ledger-report-save)
        (should (string= "AnotherName" ledger-report-name))
        (should (equal (cadr (assoc "AnotherName" ledger-reports))
                       "ledger bal new"))))))


;;; ledger-report-cmd / change-month -----------------------------------

(ert-deftest ledger-report/cmd-existing-report ()
  (let ((ledger-reports '(("Hi" "ledger bal Foo")))
        (ledger-report-name "Hi")
        (ledger-report-saved nil)
        (ledger-report-format-specifiers nil)
        (ledger-report-cmd nil))
    (cl-letf (((symbol-function 'ledger-reports-custom-save)
               (lambda () nil)))
      (let ((cmd (ledger-report-cmd "Hi" nil)))
        (should (string= cmd "ledger bal Foo"))))))

(ert-deftest ledger-report/cmd-new-report-prompts ()
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _) "ledger reg"))
            ((symbol-function 'ledger-reports-custom-save)
             (lambda () nil)))
    (let ((ledger-reports nil)
          (ledger-report-format-specifiers nil)
          (ledger-report-saved nil))
      (let ((cmd (ledger-report-cmd "NewReport" nil)))
        (should (string= cmd "ledger reg"))
        (should (assoc "NewReport" ledger-reports))))))

(ert-deftest ledger-report/change-month ()
  ;; Bind ledger-report-ledger-buf to a real buffer for `expand-format-specifiers'.
  (let ((report-buf (get-buffer-create ledger-report-buffer-name))
        (src-buf (current-buffer)))
    (unwind-protect
        (let ((ledger-report-current-month '(2024 . 5))
              (ledger-report-name "X")
              (ledger-report-ledger-buf src-buf)
              (ledger-reports '(("X" "ledger reg --period %(month)")))
              (ledger-report-format-specifiers
               '(("month" . ledger-report-month-format-specifier))))
          (cl-letf (((symbol-function 'ledger-report-redo) (lambda (&rest _) nil))
                    ((symbol-function 'ledger-reports-custom-save)
                     (lambda () nil)))
            (ledger-report--change-month -1)
            (should (equal '(2024 . 4) ledger-report-current-month))
            (ledger-report--change-month 5)
            (should (equal '(2024 . 9) ledger-report-current-month))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer report-buf)))))

(ert-deftest ledger-report/previous-and-next-month ()
  (let ((report-buf (get-buffer-create ledger-report-buffer-name))
        (src-buf (current-buffer)))
    (unwind-protect
        (let ((ledger-report-current-month '(2024 . 6))
              (ledger-report-name "X")
              (ledger-report-ledger-buf src-buf)
              (ledger-reports '(("X" "ledger reg --period %(month)")))
              (ledger-report-format-specifiers
               '(("month" . ledger-report-month-format-specifier))))
          (cl-letf (((symbol-function 'ledger-report-redo) (lambda (&rest _) nil))
                    ((symbol-function 'ledger-reports-custom-save)
                     (lambda () nil)))
            (ledger-report-previous-month)
            (should (equal '(2024 . 5) ledger-report-current-month))
            (ledger-report-next-month)
            (should (equal '(2024 . 6) ledger-report-current-month))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer report-buf)))))


;;; toggle-default-commodity --------------------------------------------

(ert-deftest ledger-report/toggle-commodity-not-in-mode ()
  (with-temp-buffer
    (should-error (ledger-report-toggle-default-commodity))))

(ert-deftest ledger-report/toggle-commodity-add ()
  (with-temp-buffer
    (ledger-report-mode)
    (let ((ledger-report-cmd "ledger reg")
          (ledger-reconcile-default-commodity "USD"))
      (cl-letf (((symbol-function 'ledger-report-redo) (lambda (&rest _) nil)))
        (ledger-report-toggle-default-commodity)
        (should (string-match-p "--exchange USD" ledger-report-cmd))
        ;; Toggle off again.
        (ledger-report-toggle-default-commodity)
        (should-not (string-match-p "--exchange USD" ledger-report-cmd))))))


;;; do-report end-to-end (mocked shell) ---------------------------------

(ert-deftest ledger-report/do-report-renders ()
  (with-temp-buffer
    (ledger-report-mode)
    (let ((ledger-report-name "Test")
          (ledger-report-use-header-line nil)
          (ledger-report-use-native-highlighting nil)
          (ledger-report-links-in-register nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (_cmd) "synthetic\noutput\n")))
        (let ((inhibit-read-only t))
          (ledger-do-report "ledger bal"))
        (let ((s (buffer-string)))
          (should (string-match-p "Report: Test" s))
          (should (string-match-p "Command: ledger bal" s))
          (should (string-match-p "synthetic" s)))))))

(ert-deftest ledger-report/do-report-strips-marker ()
  (with-temp-buffer
    (ledger-report-mode)
    (let ((ledger-report-name "T")
          (ledger-report-use-header-line nil)
          (ledger-report-use-native-highlighting nil)
          (captured nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd) (setq captured cmd) "")))
        (let ((inhibit-read-only t))
          (ledger-do-report (format "ledger %s bal" ledger-report--extra-args-marker)))
        (should captured)
        (should-not (string-match-p (regexp-quote ledger-report--extra-args-marker)
                                    captured))))))


;;; add-links --------------------------------------------------------------

(ert-deftest ledger-report/add-links-makes-buttons ()
  (with-temp-buffer
    (insert "/tmp/foo.ledger:42:  some line of register output\n"
            "no-prefix line\n")
    (goto-char (point-min))
    (ledger-report--add-links)
    (goto-char (point-min))
    ;; First line gets the ledger-source property.
    (let ((src (get-text-property (point) 'ledger-source)))
      (should (consp src))
      (should (string= "/tmp/foo.ledger" (car src)))
      (should (= 42 (cdr src))))))


;;; visit-source -----------------------------------------------------------

(ert-deftest ledger-report/visit-source-via-line-number ()
  (let ((tmp (make-temp-file "ledger-report-src-" nil ".ledger"))
        (visited-buf nil))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert "line one\nline two\nline three\n"))
          (with-temp-buffer
            (insert "x")
            (add-text-properties (point-min) (point-max)
                                 (list 'ledger-source (cons tmp 2)))
            (goto-char (point-min))
            (cl-letf (((symbol-function 'find-file-other-window)
                       (lambda (path)
                         (setq visited-buf (find-file-noselect path))
                         (set-buffer visited-buf)
                         visited-buf)))
              (ledger-report-visit-source)
              (should visited-buf)
              (with-current-buffer visited-buf
                (should (= 2 (line-number-at-pos)))))))
      (when visited-buf
        (let ((kill-buffer-query-functions nil)) (kill-buffer visited-buf)))
      (when (file-exists-p tmp) (delete-file tmp)))))


;;; Existing regression test --------------------------------------------

(defvar report-test--account-format-specifier-called-p)

(defun report-test--dummy-format-specifier ()
  "Helper function for `ledger-report/test-001'."
  (setq report-test--account-format-specifier-called-p t)
  "")

(ert-deftest ledger-report/test-001 ()
  "Regression test for #424.
https://github.com/ledger/ledger-mode/issues/424"
  :tags '(report regress)

  (let ((ledger-reports
         (cons '("dummy-report-name"
                 "%(binary) -f %(ledger-file) reg --strict --period %(month) %(account)")
               ledger-reports))
        (ledger-report-format-specifiers
         (cl-list* '("account" . report-test--dummy-format-specifier)
                   ledger-report-format-specifiers))
        (report-test--account-format-specifier-called-p nil))
    (ledger-tests-with-temp-file demo-ledger
      (ledger-report "dummy-report-name" nil)
      (should report-test--account-format-specifier-called-p)
      (should (equal (buffer-local-value
                      'ledger-report-cmd
                      (get-buffer ledger-report-buffer-name))
                     (concat "ledger [[ledger-mode-flags]] -f "
                             buffer-file-name
                             " reg --strict --period "
                             (ledger-report-month-format-specifier)
                             " ''"))))))


;;; Additional fill-in tests for residual gaps -------------------------

(ert-deftest ledger-report/month-format-specifier-uses-current-month ()
  "When `ledger-report-current-month' is nil, fall back to the current date."
  (let ((buf (get-buffer-create ledger-report-buffer-name)))
    (unwind-protect
        (let ((ledger-report-current-month nil))
          ;; The function should produce a YEAR-MONTH string.
          (should (string-match-p "\\`[0-9]+-[0-9]+\\'"
                                  (ledger-report-month-format-specifier))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest ledger-report/compute-header-line-calls-header-fn ()
  (let ((called nil))
    (cl-letf (((symbol-function 'ledger-report--header-function)
               (lambda () (setq called ledger-report-cmd) "HEADER")))
      (let ((ledger-report-header-line-fn #'ledger-report--header-function))
        (should (string= "HEADER" (ledger-report--compute-header-line "ledger reg")))
        (should (string= "ledger reg" called))))))

(ert-deftest ledger-report/do-report-with-header-line ()
  ;; When ledger-report-use-header-line is t, no Report:/Command: header
  ;; lines are inserted and the header-line-format is set.
  (with-temp-buffer
    (ledger-report-mode)
    (let ((ledger-report-name "T")
          (ledger-report-use-header-line t)
          (ledger-report-use-native-highlighting nil)
          (ledger-report-links-in-register nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (_cmd) "data\n")))
        (let ((inhibit-read-only t))
          (ledger-do-report "ledger bal"))
        (should-not (string-match-p "^Report: " (buffer-string)))
        (should header-line-format)))))

(ert-deftest ledger-report/quit-with-buffer ()
  "`ledger-report-quit' should kill the report buffer when one exists."
  (let ((rbuf (get-buffer-create ledger-report-buffer-name)))
    (with-current-buffer rbuf (ledger-report-mode))
    (cl-letf (((symbol-function 'quit-windows-on)
               (lambda (_buf _kill)
                 (let ((kill-buffer-query-functions nil))
                   (kill-buffer ledger-report-buffer-name)))))
      (ledger-report-quit)
      (should-not (get-buffer ledger-report-buffer-name)))))

(ert-deftest ledger-report/redo-not-in-mode ()
  "`ledger-report-redo' errors when invoked outside a ledger or report buffer."
  (with-temp-buffer
    (text-mode)
    (should-error (ledger-report-redo))))

(ert-deftest ledger-report/redo-no-report-buffer-noop ()
  "`ledger-report-redo' is a no-op if no report buffer exists."
  (when (get-buffer ledger-report-buffer-name)
    (let ((kill-buffer-query-functions nil))
      (kill-buffer ledger-report-buffer-name)))
  (with-temp-buffer
    (ledger-report-mode)
    ;; Should not error — just a no-op.
    (should-not (ledger-report-redo))))

(ert-deftest ledger-report/redo-runs-in-report-buffer ()
  "`ledger-report-redo' rebuilds the report from `ledger-report-cmd'."
  (let ((rbuf (get-buffer-create ledger-report-buffer-name))
        (calls 0))
    (unwind-protect
        (with-current-buffer rbuf
          (ledger-report-mode)
          (setq-local ledger-report-cmd "ledger bal")
          (setq-local ledger-report-name "X")
          (setq-local ledger-report-is-reversed nil)
          (cl-letf (((symbol-function 'shell-command-to-string)
                     (lambda (_cmd) (cl-incf calls) "out\n"))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) (set-buffer b) b)))
            (ledger-report-redo)
            (should (>= calls 1))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer rbuf)))))

(ert-deftest ledger-report/main-runs-end-to-end ()
  "Drive the main `ledger-report' entry point through a synthetic command."
  (let ((tmp (make-temp-file "ledger-report-main-" nil ".ledger")))
    (unwind-protect
        (let ((ledger-reports '(("MainTest" "ledger bal")))
              (ledger-report-format-specifiers nil))
          (cl-letf (((symbol-function 'shell-command-to-string)
                     (lambda (_cmd) "synthetic balance\n"))
                    ((symbol-function 'ledger-reports-custom-save)
                     (lambda () nil))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) (set-buffer b) b)))
            (with-current-buffer (find-file-noselect tmp)
              (ledger-mode)
              (insert "; placeholder\n")
              (ledger-report "MainTest" nil)
              (let ((rbuf (get-buffer ledger-report-buffer-name)))
                (should rbuf)
                (with-current-buffer rbuf
                  (should (string-match-p "synthetic balance"
                                          (buffer-string))))))))
      (when (get-buffer ledger-report-buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer ledger-report-buffer-name)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest ledger-report/main-interactive-form-no-modification ()
  "The interactive form's `(buffer-modified-p)' branch is harmless when not modified."
  (let ((tmp (make-temp-file "ledger-report-int-" nil ".ledger")))
    (unwind-protect
        (with-current-buffer (find-file-noselect tmp)
          (ledger-mode)
          (set-buffer-modified-p nil)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "Bal"))
                    ((symbol-function 'shell-command-to-string)
                     (lambda (_cmd) "data\n"))
                    ((symbol-function 'ledger-reports-custom-save)
                     (lambda () nil))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) (set-buffer b) b))
                    ((symbol-function 'read-from-minibuffer)
                     (lambda (&rest _) "ledger bal")))
            (let ((ledger-reports '(("Bal" "ledger bal"))))
              (call-interactively #'ledger-report)
              (should (get-buffer ledger-report-buffer-name)))))
      (when (get-buffer ledger-report-buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer ledger-report-buffer-name)))
      (when (file-exists-p tmp) (delete-file tmp)))))


(provide 'report-test)

;;; report-test.el ends here
