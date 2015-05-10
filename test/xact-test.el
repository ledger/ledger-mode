;;; xact-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-xact

;;; Code:
(require 'test-helper)


(ert-deftest ledger-xact/test-001 ()
  "Regress test for Bug 952+936+183
http://bugs.ledger-cli.org/show_bug.cgi?id=952
http://bugs.ledger-cli.org/show_bug.cgi?id=936
http://bugs.ledger-cli.org/show_bug.cgi?id=183"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
   "2013/05/01 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
"
   (goto-char (point-max))              ; end-of-buffer
   (ledger-add-transaction "2013/05/02 foo")
   (should
    (equal (buffer-string)
           "2013/05/01 foo
    Expenses:Foo                            $10.00
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                              $10.00
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                            $10.00
    Assets:Bar
" ))))


(ert-deftest ledger-xact/test-002 ()
  "Regress test for Bug 526
http://bugs.ledger-cli.org/show_bug.cgi?id=526"
  :tags '(xact regress)

  (ledger-tests-with-temp-file
   "2013/05/01 foo
    Expenses:Foo                             10,00 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             10,00 €
    Assets:Bar
"
   (goto-char (point-max))              ; end-of-buffer
   (ledger-add-transaction "2013/05/02 foo 16,02")
   (should
    (equal (buffer-string)
           "2013/05/01 foo
    Expenses:Foo                             10,00 €
    Assets:Bar

2013/05/02 foo
    Expenses:Foo                               16,02 €
    Assets:Bar

2013/05/03 foo
    Expenses:Foo                             10,00 €
    Assets:Bar
"))))


(provide 'xact-test)

;;; xact-test.el ends here
