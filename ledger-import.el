;;; ledger-import.el --- Fetch and import OFX files into Ledger  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ledger-mode)

(defgroup ledger-import nil
  "Fetch and import OFX files into Ledger."
  :group 'ledger)

(defcustom ledger-import-accounts nil
  "Alist mapping OFX account names to Ledger account names."
  :group 'ledger-import
  :type `(alist
          :key-type
          (string
           :tag "OFX account name"
           :match ledger-import--non-empty-string-widget-matcher
           :doc "Account name as known by boobank"
           :format "%t: %v%h\n")
          :value-type
          (group
           (string
            :tag "Ledger account"
            :match ledger-import--non-empty-string-widget-matcher
            :doc "Account name (e.g., \"Assets:Current\") as known by Ledger"
            :format "%t: %v%h\n")
           (string
            :tag "FID"
            :value ""
            :doc "Use only if ledger-autosync complains about missing FID"
            :format "%t: %v%h\n")
           (radio
            :tag "Fetcher"
            :value boobank
            :doc "Tool to use to get the account's OFX file"
            :format "%t: %v%h\n"
            (const :tag "Boobank" boobank)))))

(defcustom ledger-import-autosync-command '("ledger-autosync" "--assertions")
  "List of strings with ledger-autosync command name and arguments."
  :group 'ledger-import
  :type '(repeat string))

(defcustom ledger-import-boobank-command '("boobank")
  "List of strings with boobank command name and arguments."
  :group 'ledger-import
  :type '(repeat string))

(defcustom ledger-import-boobank-import-from-date "2018-10-01"
  "String representing a date from which to import OFX data with boobank."
  :group 'ledger-import
  :type 'string)

(defcustom ledger-import-fetched-hook nil
  "Hook run when an OFX file is ready to be imported.
The OFX buffer is made current before the hook is run."
  :group 'ledger-import
  :type 'hook)

(defcustom ledger-import-finished-hook nil
  "Hook run when all transactions have been imported.
The `ledger-import-buffer' is made current before the hook is run."
  :group 'ledger-import
  :type 'hook)

(defun ledger-import-buffer ()
  "Return the buffer containing imported transactions."
  (get-buffer-create "*ledger sync*"))

(defun ledger-import-pop-to-buffer (&optional buffer)
  "Make BUFFER visible, `ledger-import-buffer' if nil."
  (interactive)
  (pop-to-buffer-same-window (or buffer (ledger-import-buffer))))

(defun ledger-import--non-empty-string-widget-matcher (_widget value)
  "Return non-nil if VALUE is a non-empty string."
  (message "value: %s" value)
  (and (stringp value) (> (length value) 0)))

(defun ledger-import-account-ofx-name (account)
  "Return OFX account name for ACCOUNT as known by the importer."
  (nth 0 account))

(defun ledger-import-account-ledger-name (account)
  "Return Ledger account name for ACCOUNT as known by the fetcher."
  (nth 1 account))

(defun ledger-import-account-fid (account)
  "Return FID for ACCOUNT."
  (let ((fid (nth 2 account)))
    (if (or (null fid) (string= fid ""))
        nil
      fid)))

(defun ledger-import-choose-account ()
  "Ask the user to choose an account among `ledger-import-accounts'."
  (let* ((accounts ledger-import-accounts)
         (account-name (completing-read "Ledger account: "
                                        (mapcar #'ledger-import-account-ledger-name accounts)
                                        nil
                                        t)))
    (seq-find (lambda (account) (string= (ledger-import-account-ledger-name account)
                                    account-name))
              accounts)))

(defun ledger-import-ofx-import-file (account in-buffer &optional callback ledger-file)
  "Import ofx data for ACCOUNT from IN-BUFFER with ledger-autosync.
Display result in `ledger-import-buffer' and execute CALLBACK when done.

If LEDGER-FILE is non nil, use transactions from this file to
guess related account names."
  (interactive (list (ledger-import-choose-account) (current-buffer) #'ledger-import-pop-to-buffer))
  (with-current-buffer in-buffer
    (let* ((ledger-name (ledger-import-account-ledger-name account))
           (fid (ledger-import-account-fid account))
           (file (make-temp-file "ledger-import-" nil ".ledger"))
           (command `(,@ledger-import-autosync-command
                      ,@(when ledger-file `("--ledger" ,ledger-file))
                      "--account" ,ledger-name
                      ,@(when fid `("--fid" ,fid))
                      ,file)))
      (write-region nil nil file nil 'no-message)
      (message "Starting ledger-autosync for %s" ledger-name)
      (make-process
       :name "ledger-autosync"
       :buffer (ledger-import-buffer)
       :command command
       :sentinel (lambda (_process event)
                   (when (and callback (string= event "finished\n"))
                     (funcall callback))
                   (when (string-prefix-p "exited abnormally" event)
                     (pop-to-buffer-same-window (ledger-import-buffer))
                     (error "There was a problem with ledger-autosync while importing %s" ledger-name)))))))

(defun ledger-import-ofx-fetch-boobank (account &optional callback retry)
  "Use boobank to fetch OFX data for ACCOUNT.
When done, execute CALLBACK with buffer containing OFX data.

RETRY is a number (default 3) indicating the number of times
boobank is executed if it fails.  This is because boobank tends
to fail often and restarting usually solves the problem."
  (interactive (list (ledger-import-account-ofx-name (ledger-import-choose-account)) #'ledger-import-pop-to-buffer))
  (let ((retry (or retry 3))
        (buffer (generate-new-buffer (format "*ledger-import-%s*" account)))
        (error-buffer (generate-new-buffer (format "*ledger-import-%s <stderr>*" account)))
        (command `(,@ledger-import-boobank-command
                   "--formatter=ofx"
                   "history"
                   ,account
                   ,ledger-import-boobank-import-from-date)))
    (with-current-buffer buffer
      (message "Starting boobank for %s" account)
      (make-process
       :name (format "boobank %s" account)
       :buffer buffer
       :stderr error-buffer
       :command command
       :sentinel (lambda (_process event)
                   (when (string= event "finished\n")
                     (if (not (with-current-buffer error-buffer (= (point-min) (point-max))))
                         (ledger-import--ofx-fetch-boobank-error retry account callback error-buffer)
                       (kill-buffer error-buffer)
                       (with-current-buffer buffer (run-hooks 'ledger-import-fetched-hook))
                       (when callback (funcall callback buffer))))
                   (when (string-prefix-p "exited abnormally" event)
                     (ledger-import--ofx-fetch-boobank-error retry account callback error-buffer)))))))

(defun ledger-import--ofx-fetch-boobank-error (retry account callback error-buffer)
  "Throw an error if RETRY is 0 or try starting boobank again.

ACCOUNT and CALLBACK are the same as in `ledger-import-ofx-fetch-boobank'.

ERROR-BUFFER is a buffer containing an error message explaining the problem."
  (if (>= retry 0)
      (ledger-import-ofx-fetch-boobank account callback (1- retry))
    (pop-to-buffer-same-window error-buffer)
    (error "There was a problem with boobank while importing %s" account)))

(defun ledger-import--account (account &optional callback ledger-file)
  "Import all of ACCOUNT and put the result in `ledger-import-buffer'.
When done, execute CALLBACK.

If LEDGER-FILE is non nil, use transactions from this file to
guess related account names."
  (ledger-import-ofx-fetch-boobank
   (ledger-import-account-ofx-name account)
   (lambda (ofx-buffer)
     (ledger-import-ofx-import-file
      account
      ofx-buffer
      (lambda ()
        (kill-buffer ofx-buffer)
        (when callback
          (funcall callback)))
      ledger-file))))

(defun ledger-import--accounts (accounts &optional callback ledger-file)
  "Import all of ACCOUNTS and put the result in `ledger-import-buffer'.
When done, execute CALLBACK.

If LEDGER-FILE is non nil, use transactions from this file to
guess related account names."
  (if (null accounts)
      (when callback (funcall callback))
    (ledger-import--account
     (car accounts)
     (lambda ()
       (ledger-import--accounts (cdr accounts) callback ledger-file))
     ledger-file)))

(defun ledger-import-all-accounts (&optional ledger-file)
  "Import transactions from ofx to Ledger format using \"ledger-autosync\".

If LEDGER-FILE is non nil, use transactions from this file to
guess related account names."
  (interactive (list
                (when (and (buffer-file-name) (derived-mode-p 'ledger-mode))
                  (buffer-file-name))))
  (require 'ledger-mode)
  (let ((buffer (ledger-import-buffer)))
    (with-current-buffer buffer (erase-buffer))
    (ledger-import--accounts
     ledger-import-accounts
     (lambda ()
       (with-current-buffer buffer
         (ledger-mode)
         (ledger-mode-clean-buffer)
         (run-hooks 'ledger-import-finished-hook)))
     ledger-file)))

(provide 'ledger-import)
;;; ledger-import.el ends here

;; Local Variables:
;; eval: (flycheck-mode)
;; End:
