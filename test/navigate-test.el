;;; navigate-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-navigate

;;; Code:
(require 'test-helper)


(ert-deftest ledger-navigate/test-001 ()
  "Regress test for Bug 441
http://bugs.ledger-cli.org/show_bug.cgi?id=441"
  :tags '(navigate regress)

  (ledger-tests-with-temp-file
   demo-ledger
   (goto-char (point-min))              ; beginning-of-buffer
   (ledger-navigate-next-xact-or-directive)
   (ledger-navigate-next-xact-or-directive)
   (should (eq 556 (point)))
   (ledger-navigate-prev-xact-or-directive)
   (should (eq 104 (point)))))


(provide 'navigate-test)

;;; navigate-test.el ends here
