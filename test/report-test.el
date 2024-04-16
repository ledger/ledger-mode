;;; report-test.el --- ERT for ledger-report  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 John Wiegley <johnw AT gnu DOT org>

;; Author: Damien Cassou <damien@cassou.me>
;; Homepage: https://github.com/ledger/ledger-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  Regression tests for ledger-report

;;; Code:
(require 'test-helper)


(ert-deftest ledger-report/normalize-month ()
  "Test ledger-report--normalize-month"
  :tags '(report)

  (should (equal (ledger-report--normalize-month '(2018 . 10)) '(2018 . 10)))
  (should (equal (ledger-report--normalize-month '(2018 . 0)) '(2017 . 12)))
  (should (equal (ledger-report--normalize-month '(2018 . -2)) '(2017 . 10)))
  (should (equal (ledger-report--normalize-month '(2018 . 13)) '(2019 . 1)))
  (should (equal (ledger-report--normalize-month '(2018 . 12)) '(2018 . 12)))
  (should (equal (ledger-report--normalize-month '(2018 . -12)) '(2016 . 12))))

(ert-deftest ledger-report/shift-month ()
  "Test ledger-report--shift-month"
  :tags '(report)

  (should (equal (ledger-report--shift-month '(2018 . 10) 2) '(2018 . 12)))
  (should (equal (ledger-report--shift-month '(2018 . 10) 3) '(2019 . 1)))
  (should (equal (ledger-report--shift-month '(2018 . 1) -1) '(2017 . 12)))
  (should (equal (ledger-report--shift-month '(2018 . 1) -13) '(2016 . 12))))

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

(provide 'report-test)

;;; report-test.el ends here
