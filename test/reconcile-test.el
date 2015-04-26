;;; reconcile-test.el --- ERT for ledger-mode

;;; Commentary:
;;  Regression tests for ledger-reconcile

;;; Code:
(require 'test-helper)


(ert-deftest ledger-reconcile/test-001 ()
  "Regress test for Bug 1107
http://bugs.ledger-cli.org/show_bug.cgi?id=1107"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; this moves to *recon* buffer
    (other-window 1)                ; go to *ledger* buffer
    (insert " ")                    ; simulate modification of ledger buffer
    (delete-char -1)
    (other-window 1)                ; back to *reconcile* buffer
    (ledger-reconcile-save)         ; key 's'
    (should ;; current buffer should be *recon* buffer
     (equal (buffer-name)           ; current buffer name
            ledger-recon-buffer-name))
    (other-window 1)                ; switch to *other* window
    (should ;; Expected: this must be ledger buffer
     (equal (buffer-name)           ; current buffer name
            (buffer-name ledger-buffer)))))


(ert-deftest ledger-reconcile/test-002 ()
  "Regress test for Bug 1060
http://bugs.ledger-cli.org/show_bug.cgi?id=1060"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-recon-buffer-name)) ; IRL user select recon window
    (forward-line 2)                    ; because of ledger-reconcile-buffer-header
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-finish)                     ; C-c C-c
    (should ;; Expected: buffer recon must still exist and be selected
     (equal ledger-recon-buffer-name
            (buffer-name (window-buffer (selected-window)))))))


(ert-deftest ledger-reconcile/test-003 ()
  "Regress test for Bug 1060
http://bugs.ledger-cli.org/show_bug.cgi?id=1060

If `ledger-reconcile-finish-force-quit' is set, recon window is killed"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
      demo-ledger
    (setq ledger-reconcile-finish-force-quit t)
    (ledger-reconcile "Assets:Checking" '(0 "$")) ; launch reconciliation
    (select-window (get-buffer-window ledger-recon-buffer-name)) ; IRL user select recon window
    (forward-line 2)                    ; because of ledger-reconcile-buffer-header
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-toggle)                     ; mark pending
    (ledger-reconcile-finish)                     ; C-c C-c
    (should ;; Expected: recon buffer has been killed
     (equal nil (get-buffer-window ledger-recon-buffer-name)))))


(ert-deftest ledger-reconcile/test-004 ()
  "Regress test for Bug 1108
http://bugs.ledger-cli.org/show_bug.cgi?id=1108"
  :tags '(reconcile regress)

  (ledger-tests-with-temp-file
"2014/11/10  EDF
    Dépense:Maison:Service:Électricité    36,23 €
    Actif:Courant:BnpCc

2014/11/14  Banque Accord Retrait
    Passif:Crédit:BanqueAccord              60,00 €
    Actif:Courant:BnpCc
"
    (goto-char 150)                 ; line 6, before A of BanqueAccord
    (let ((context (ledger-context-at-point)))
      (should (eq (ledger-context-current-field context) 'account))
      (should (equal "Passif:Crédit:BanqueAccord"
                     (ledger-context-field-value context 'account))))))


(provide 'reconcile-test)

;;; reconcile-test.el ends here
