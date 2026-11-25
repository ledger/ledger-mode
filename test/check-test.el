;;; check-test.el --- ERT for ledger-check  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2025 John Wiegley <johnw AT gnu DOT org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;;; Commentary:
;;  Regression tests for ledger-check.

;;; Code:
(require 'test-helper)
(require 'ledger-check)

(ert-deftest ledger-check/mode-keymap-bindings ()
  "RET visits source, q quits."
  (should (eq (lookup-key ledger-check-mode-map (kbd "RET"))
              #'ledger-report-visit-source))
  (should (eq (lookup-key ledger-check-mode-map (kbd "q"))
              #'ledger-check-quit)))

(ert-deftest ledger-check/check-goto-without-buffer ()
  "Calling `ledger-check-goto' when no check buffer exists errors."
  (when (get-buffer ledger-check-buffer-name)
    (kill-buffer ledger-check-buffer-name))
  (should-error (ledger-check-goto)))

(ert-deftest ledger-check/check-goto-pops-to-buffer ()
  "When the check buffer exists, `ledger-check-goto' pops to it."
  (let ((buf (get-buffer-create ledger-check-buffer-name)))
    (unwind-protect
        (progn
          (ledger-check-goto)
          (should (eq (current-buffer) buf)))
      (kill-buffer buf))))

(ert-deftest ledger-check/do-check-no-errors ()
  "`ledger-do-check' inserts a 'no warnings' note when ledger reports nothing."
  ;; TODO: This test should succeed once ledger-do-check starts actually reading
  ;; from the current buffer.  For now, it does not pass any -f argument to the
  ;; ledger binary so falls back to whatever is specified in ~/.ledgerrc.
  :expected-result :failed
  (ledger-tests-with-temp-file ""
    (ledger-do-check)
    (should (equal "No errors or warnings reported.\n"
                   (buffer-string)))))

(ert-deftest ledger-check/do-check-parses-error-line ()
  "An error line is decorated with `ledger-source' text properties."
  (ledger-tests-with-temp-file
      "\
2024/01/01 Acme
    Expenses:Food   $10
    Assets:Cash    -$5
"
    (let ((src-buffer (current-buffer)))
      (ledger-check-buffer)
      (with-current-buffer ledger-check-buffer-name
        ;; The marked line should carry a 'ledger-source text property
        ;; whose value is a marker in the source buffer.
        (goto-char (point-min))
        (let ((src (get-text-property (point) 'ledger-source)))
          (should (markerp src))
          (should (eq (marker-buffer src) src-buffer))
          (should (equal (with-current-buffer src-buffer
                           (line-number-at-pos src))
                         2)))))))

(ert-deftest ledger-check/check-buffer-creates-output-buffer ()
  "`ledger-check-buffer' switches to the check buffer in `ledger-check-mode'."
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_cmd &rest _) nil))
            ;; ledger-master-file calls find-file-noselect, which we route
            ;; to a temp.
            ((symbol-function 'ledger-master-file)
             (lambda () (or buffer-file-name "/tmp/master.ledger"))))
    (let* ((tmp (make-temp-file "ledger-check-master-")))
      (unwind-protect
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (insert "2024/01/01 Acme\n  Expenses:Food  $1\n  Assets:Cash\n")
            (write-region (point-min) (point-max) tmp)
            (set-buffer-modified-p nil)
            ;; non-interactive: passes nil, so no save prompt.
            (ledger-check-buffer nil)
            (let ((cb (get-buffer ledger-check-buffer-name)))
              (should cb)
              (with-current-buffer cb
                (should (eq major-mode 'ledger-check-mode))
                (should buffer-read-only)
                (should ledger-check--original-window-configuration))))
        (when (file-exists-p tmp) (delete-file tmp))
        (when (get-buffer ledger-check-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer ledger-check-buffer-name)))))))

(ert-deftest ledger-check/check-quit-restores-window-config ()
  "`ledger-check-quit' calls `set-window-configuration' and kills the buffer."
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_cmd &rest _) nil))
            ((symbol-function 'ledger-master-file)
             (lambda () (or buffer-file-name "/tmp/master.ledger"))))
    (let* ((tmp (make-temp-file "ledger-check-master-")))
      (unwind-protect
          (with-temp-buffer
            (setq buffer-file-name tmp)
            (insert "")
            (write-region (point-min) (point-max) tmp)
            (set-buffer-modified-p nil)
            (ledger-check-buffer nil)
            (should (get-buffer ledger-check-buffer-name))
            (cl-letf (((symbol-function 'set-window-configuration)
                       (lambda (_cfg) nil)))
              (ledger-check-quit))
            (should-not (get-buffer ledger-check-buffer-name)))
        (when (file-exists-p tmp) (delete-file tmp))
        (when (get-buffer ledger-check-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer ledger-check-buffer-name)))))))


(ert-deftest ledger-check/check-buffer-saves-modified-when-prompted ()
  "When called interactively with a modified buffer and the user accepts,
`ledger-check-buffer' saves the buffer before running."
  (let ((saved nil))
    (cl-letf (((symbol-function 'shell-command)
               (lambda (_cmd &rest _) nil))
              ((symbol-function 'ledger-master-file)
               (lambda () (or buffer-file-name "/tmp/master.ledger")))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'save-buffer)
               (lambda (&rest _) (setq saved t))))
      (let ((tmp (make-temp-file "ledger-check-modified-")))
        (unwind-protect
            (with-temp-buffer
              (setq buffer-file-name tmp)
              (insert "x")
              (set-buffer-modified-p t)
              (ledger-check-buffer 1)
              (should saved))
          (when (file-exists-p tmp) (delete-file tmp))
          (when (get-buffer ledger-check-buffer-name)
            (let ((kill-buffer-query-functions nil))
              (kill-buffer ledger-check-buffer-name))))))))

(ert-deftest ledger-check/check-buffer-kills-existing-output ()
  "If the check buffer already exists, `ledger-check-buffer' kills it before
re-running."
  (cl-letf (((symbol-function 'shell-command)
             (lambda (_cmd &rest _) nil))
            ((symbol-function 'ledger-master-file)
             (lambda () (or buffer-file-name "/tmp/master.ledger"))))
    (let ((tmp (make-temp-file "ledger-check-existing-")))
      (unwind-protect
          (let ((stale (get-buffer-create ledger-check-buffer-name)))
            (with-current-buffer stale
              (insert "stale content"))
            (with-temp-buffer
              (setq buffer-file-name tmp)
              (insert "")
              (write-region (point-min) (point-max) tmp)
              (set-buffer-modified-p nil)
              (ledger-check-buffer nil))
            ;; The new buffer should exist and be empty of stale content.
            (let ((cur (get-buffer ledger-check-buffer-name)))
              (should cur)
              (with-current-buffer cur
                (should-not (string-match-p "stale content" (buffer-string))))))
        (when (file-exists-p tmp) (delete-file tmp))
        (when (get-buffer ledger-check-buffer-name)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer ledger-check-buffer-name)))))))


(provide 'check-test)

;;; check-test.el ends here
