;;; occur-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-occur

;;; Code:
(require 'test-helper)


(ert-deftest ledger-occur/test-001 ()
  "Regress test for Bug 246
http://bugs.ledger-cli.org/show_bug.cgi?id=246"
  :tags '(occur regress)

  (ledger-tests-with-temp-file
   "2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking

2011/01/05 Employer
  * Assets:Checking                 $ 2000.00
  Income:Salary
"
   (ledger-occur "Groceries")
   (should
    (equal (ledger-test-visible-buffer-string)
           "2011/01/02 Grocery Store
  Expenses:Food:Groceries             $ 65.00
  * Assets:Checking
"))))


(provide 'occur-test)

;;; occur-test.el ends here
