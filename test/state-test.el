;;; state-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-state

;;; Code:
(require 'test-helper)


(ert-deftest ledger-state/test-001 ()
  "Regress test for Bug 1030
http://bugs.ledger-cli.org/show_bug.cgi?id=1030"
  :tags '(state regress)

  (ledger-tests-with-temp-file
"2013/06/09 * 4 Saisons
    Dépense:Alimentation:Restaurant          23,00 €
    Passif:Crédit:BanqueAccord
"
    (forward-line 1)
    (ledger-toggle-current)             ; C-c C-c
    (should
     (equal (buffer-string)
      "2013/06/09 4 Saisons
    Dépense:Alimentation:Restaurant          23,00 €
    * Passif:Crédit:BanqueAccord
" ))
    (ledger-toggle-current)             ; C-c C-c
    (should
     (equal (buffer-string)
      "2013/06/09 * 4 Saisons
    Dépense:Alimentation:Restaurant          23,00 €
    Passif:Crédit:BanqueAccord
" ))))


(provide 'state-test)

;;; state-test.el ends here
