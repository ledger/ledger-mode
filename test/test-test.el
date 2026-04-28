;;; test-test.el --- ERT for ledger-test  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2025 John Wiegley <johnw AT gnu DOT org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;;; Commentary:
;;  Regression tests for ledger-test (the regression-test extraction module).

;;; Code:
(require 'test-helper)
(require 'ledger-test)
(require 'org)


(defun ledger-test-test--make-org-buffer ()
  "Return a fresh org buffer with a single subtree containing a ledger test."
  (let ((buf (generate-new-buffer "*ledger-test-test*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* Sample Test
:PROPERTIES:
:ID:       abc1234-5678-9abc-def0-000000000001
:END:

#+begin_src ledger
2024/04/27 Coffee Shop
    Expenses:Food                $4
    Assets:Checking
#+end_src

:OUTPUT:
some output line
:END:
"))
    buf))


(ert-deftest ledger-test/test-org-narrow-to-entry ()
  "`ledger-test-org-narrow-to-entry' should restrict to the current heading."
  (let ((buf (ledger-test-test--make-org-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (save-restriction
            (ledger-test-org-narrow-to-entry)
            ;; Restriction should start at the heading.
            (should (looking-at-p "\\* Sample Test"))
            ;; The entire buffer (one subtree) ends up inside the restriction
            ;; — point-min and point-max bracket the visible region.
            (should (< (point-min) (point-max)))))
      (kill-buffer buf))))


(defvar ledger-test-test--captured-buffer nil
  "Used by `ledger-test/test-create-*' tests to capture the buffer that
`ledger-test-create' would have opened in another window.")

(defun ledger-test-test--capture-find-file (path)
  "Stand-in for `find-file-other-window' that records the visited buffer."
  (let ((buf (find-file-noselect path)))
    (setq ledger-test-test--captured-buffer buf)
    (set-buffer buf)
    buf))

(ert-deftest ledger-test/test-create-extracts-block ()
  "`ledger-test-create' extracts the source/output blocks into a *.test buffer."
  (let ((buf (ledger-test-test--make-org-buffer))
        (tmp-source-dir (file-name-as-directory
                         (make-temp-file "ledger-source-" t)))
        (ledger-test-test--captured-buffer nil))
    (unwind-protect
        (let* ((ledger-source-directory tmp-source-dir)
               (test-dir (expand-file-name "test/regress" tmp-source-dir)))
          (make-directory test-dir t)
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward ":END:")
            (cl-letf (((symbol-function 'find-file-other-window)
                       #'ledger-test-test--capture-find-file))
              (ledger-test-create)))
          (should ledger-test-test--captured-buffer)
          (with-current-buffer ledger-test-test--captured-buffer
            (let ((s (buffer-string)))
              (should (string-match-p "Coffee Shop" s))
              (should (string-match-p "test reg" s))
              (should (string-match-p "end test" s))))
          (kill-buffer ledger-test-test--captured-buffer))
      (kill-buffer buf)
      (delete-directory tmp-source-dir t))))


(ert-deftest ledger-test/test-create-no-src-block ()
  "`ledger-test-create' with no #+begin_src block emits the default xact."
  (let ((buf (generate-new-buffer "*ledger-test-test-default*"))
        (tmp-source-dir (file-name-as-directory
                         (make-temp-file "ledger-source-" t)))
        (ledger-test-test--captured-buffer nil))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (insert "* Default Test
:PROPERTIES:
:ID:       def5678-aaaa-bbbb-cccc-000000000002
:END:
")
          (let ((ledger-source-directory tmp-source-dir))
            (make-directory (expand-file-name "test/regress" tmp-source-dir) t)
            (goto-char (point-min))
            (re-search-forward ":END:")
            (cl-letf (((symbol-function 'find-file-other-window)
                       #'ledger-test-test--capture-find-file))
              (ledger-test-create))
            (should ledger-test-test--captured-buffer)
            (with-current-buffer ledger-test-test--captured-buffer
              (let ((s (buffer-string)))
                (should (string-match-p "2012-03-17 Payee" s))
                (should (string-match-p "test reg" s))
                (should (string-match-p "end test" s))))
            (kill-buffer ledger-test-test--captured-buffer)))
      (kill-buffer buf)
      (delete-directory tmp-source-dir t))))


(ert-deftest ledger-test/test-run-builds-args ()
  "`ledger-test-run' should build the expected ledger CLI invocation."
  (let ((captured nil)
        (src-dir (file-name-as-directory (make-temp-file "ledger-source-" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'async-shell-command)
                   (lambda (cmd) (setq captured cmd))))
          (with-temp-buffer
            (let ((buffer-file-name (expand-file-name "example.test" src-dir))
                  (default-directory src-dir)
                  (ledger-source-directory src-dir)
                  (ledger-test-binary "/tmp/ledger-bin"))
              (insert "test reg --base-currency=USD\n"
                      "ledger output line\n"
                      "end test\n")
              (ledger-test-run)
              (should captured)
              (should (string-match-p "reg --base-currency=USD" captured))
              (should (string-match-p "/tmp/ledger-bin" captured))
              (should (string-match-p "example.test" captured)))))
      (delete-directory src-dir t))))


(ert-deftest ledger-test/test-run-replaces-sourcepath ()
  "`$sourcepath' in test args is substituted for `ledger-source-directory'."
  (let ((captured nil)
        (src-dir (file-name-as-directory (make-temp-file "ledger-source-" t))))
    (unwind-protect
        (cl-letf (((symbol-function 'async-shell-command)
                   (lambda (cmd) (setq captured cmd))))
          (with-temp-buffer
            (let ((buffer-file-name (expand-file-name "sub.test" src-dir))
                  (default-directory src-dir)
                  (ledger-source-directory src-dir)
                  (ledger-test-binary "/tmp/ledger-bin"))
              (insert "test reg --price-db $sourcepath/db\n"
                      "end test\n")
              (ledger-test-run)
              (should captured)
              ;; src-dir already ends in "/"; substitution yields "<dir>//db".
              (should (string-match-p (regexp-quote src-dir) captured))
              (should (string-match-p "/db" captured))
              (should-not (string-match-p "\\$sourcepath" captured)))))
      (delete-directory src-dir t))))


(ert-deftest ledger-test/test-run-no-test-line ()
  "Without a `test ...' line, `ledger-test-run' shouldn't fire ledger."
  (let ((called nil))
    (cl-letf (((symbol-function 'async-shell-command)
               (lambda (_cmd) (setq called t))))
      (with-temp-buffer
        (insert "no test directive in this buffer\n")
        (ledger-test-run)
        (should-not called)))))


(ert-deftest ledger-test/create-test-extracts-properties-and-strips-prefix ()
  "`ledger-create-test' extracts the org-mode properties drawer and inserts
it (sans `;; ' prefix) into a *.test file at `~/src/ledger/test/regress/'.
Mocks `find-file-other-window' so the hardcoded path is captured rather
than touched."
  (let ((src
         "* Some Test
;;; preamble line
;;; second preamble line
:PROPERTIES:
:ID:       feedface-9999-aaaa-bbbb-ccccccccdddd
:END:

body content
")
        (captured-path nil)
        (captured-buf nil))
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (path)
                 (setq captured-path path)
                 (setq captured-buf (generate-new-buffer "*captured*"))
                 (set-buffer captured-buf)
                 captured-buf)))
      (with-temp-buffer
        (org-mode)
        (insert src)
        (goto-char (point-min))
        (re-search-forward "^\\* ")
        (ledger-create-test))
      (unwind-protect
          (progn
            (should captured-path)
            (should (string-match-p "feedface\\.test\\'" captured-path))
            (should captured-buf)
            (with-current-buffer captured-buf
              (let ((s (buffer-string)))
                ;; The leading `;;; ' was stripped from each line.
                ;; The leading 3 chars (`;;;') are stripped from each line;
                ;; the trailing space stays.
                (should (string-match-p " preamble line" s))
                (should (string-match-p " second preamble line" s))
                ;; The properties drawer was NOT included (the function only
                ;; copies up to the :PROPERTIES: line).
                (should-not (string-match-p ":ID:" s)))))
        (when captured-buf
          (let ((kill-buffer-query-functions nil))
            (kill-buffer captured-buf)))))))


(provide 'test-test)

;;; test-test.el ends here
