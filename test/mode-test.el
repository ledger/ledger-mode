;;; mode-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-mode.el

;;; Code:
(require 'test-helper)


(ert-deftest ledger-mode/test-001 ()
  "Test ledger-read-account-with-prompt (used in ledger-reconcile)"
  :tags '(mode baseline interactive)

  (ledger-tests-with-temp-file
   demo-ledger

   (goto-char (point-min))              ; beginning-of-buffer
   ;; See http://stackoverflow.com/questions/32961823/how-can-i-test-an-interactive-function-in-emacs
   ;; for an explanation of unread-command-events trick
   (should (string= ""
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   (forward-line)
   (should (string= "Assets:Checking"
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   (forward-line)
   (should (string= "Equity:Opening Balances"
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   (forward-line)
   (should (string= ""
                    (let ((unread-command-events (listify-key-sequence (kbd "RET"))))
                      (ledger-read-account-with-prompt "Account to reconcile"))))
   ))


(provide 'mode-test)

;;; mode-test.el ends here
