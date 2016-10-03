;;; exec-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-exec

;;; Code:
(require 'test-helper)


(ert-deftest ledger-exec/test-001 ()
  "Regress test for Bug 254
http://bugs.ledger-cli.org/show_bug.cgi?id=254"
  :tags '(exec regress)

  (ledger-tests-with-temp-file
      ""
    (ledger-check-version)
    (should
     (eq t ledger-works))))


(provide 'exec-test)

;;; exec-test.el ends here
