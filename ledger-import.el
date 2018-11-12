;;; ledger-import.el --- Fetch OFX files and convert them into Ledger format  -*- lexical-binding: t; -*-

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

;; This file contains code to simplify importing new transactions into your
;; Ledger file.  Transactions are fetched using a fetcher (only boobank from the
;; weboob project is supported for now) and the OFX file format.  Then, the OFX
;; buffers are converted into Ledger format through ledger-autosync.

;; To use ledger-import, you first have to install and configure boobank, from
;; the weboob project:
;;
;; - http://weboob.org/
;; - http://weboob.org/applications/boobank
;;
;; When you manage to visualize your bank accounts with boobank, you should
;; configure each in `ledger-import-accounts'.  Use `customize-variable' to do
;; that if you want to.  You can check that your configuration works with `M-x
;; ledger-import-fetch-boobank': after a few tens of seconds, you will get a
;; buffer with OFX data.  If boobank imports transactions that are too old, you
;; can configure `ledger-import-boobank-import-from-date'.
;;
;; To convert an OFX file into Ledger format, ledger-import uses ledger-autosync
;; that you have to install as well:
;;
;; - https://github.com/egh/ledger-autosync
;;
;; This doesn't require additional configuration.  To test that ledger-autosync
;; works fine, go back to the buffer containing OFX data (or create a new one),
;; and type `M-x ledger-import-convert-ofx-to-ledger'.  After a few seconds, you
;; should get your transactions in Ledger format.  If you instead get a message
;; saying that the OFX data did not provide any FID, then you can provide a
;; random one in `ledger-import-accounts'.
;;
;; To fetch transactions from all configured accounts and convert them to Ledger
;; format, type `M-x ledger-import-all-accounts'.  When this is finished, you
;; can open the result with `M-x ledger-import-pop-to-buffer'.
;;
;; If you keep manually modifying the Ledger transactions after they have been
;; converted, you might prefer to let ledger-import do that for you.
;; ledger-import gives you 2 ways to rewrite OFX data: either through the
;; `ledger-import-fetched-hook' or through `ledger-import-ofx-rewrite-rules'.

;;; Code:

(require 'ledger-mode)

(defgroup ledger-import nil
  "Fetch OFX files and convert them into Ledger format."
  :group 'ledger)

(defcustom ledger-import-accounts nil
  "Ledger accounts for which to fetch and convert data."
  :group 'ledger-import
  :type '(repeat
          (group
           (string
            :tag "Ledger account"
            :match ledger-import--non-empty-string-widget-matcher
            :doc "Account name (e.g., \"Assets:Current\") as known by Ledger"
            :format "%t: %v%h\n")
           (radio
            :tag "Fetcher"
            :value boobank
            :doc "Tool to use to get the account's OFX file"
            :format "%t: %v%h\n"
            (const :tag "Boobank" boobank))
           (string
            :tag "Boobank account name"
            :match ledger-import--non-empty-string-widget-matcher
            :doc "Account name as known by boobank"
            :format "%t: %v%h\n")
           (string
            :tag "FID"
            :value ""
            :doc "Use only if ledger-autosync complains about missing FID"
            :format "%t: %v%h\n"))))

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
  :type '(string
          :match (lambda (_ value)
                   (string-match-p
                    "[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}"
                    value))))

(defcustom ledger-import-ofx-rewrite-rules nil
  "List of (REGEXP . REPLACEMENT) to apply in an OFX buffer."
  :group 'ledger-import
  :type '(repeat
          (cons
           (regexp :tag "What to search for" :value "")
           (string :tag "What to replace it with" :value ""))))

(defcustom ledger-import-fetched-hook '(ledger-import-ofx-rewrite)
  "Hook run when an OFX file is ready to be converted to Ledger format.
The OFX buffer is made current before the hook is run."
  :group 'ledger-import
  :type 'hook)

(defcustom ledger-import-finished-hook nil
  "Hook run when all transactions have been converted to Ledger format.
The `ledger-import-buffer' is made current before the hook is run."
  :group 'ledger-import
  :type 'hook)

(defun ledger-import-buffer ()
  "Return the buffer containing imported transactions."
  (get-buffer-create "*ledger sync*"))

;;;###autoload
(defun ledger-import-pop-to-buffer (&optional buffer)
  "Make BUFFER visible, `ledger-import-buffer' if nil."
  (interactive)
  (pop-to-buffer-same-window (or buffer (ledger-import-buffer))))

(defun ledger-import--non-empty-string-widget-matcher (_widget value)
  "Return non-nil if VALUE is a non-empty string."
  (and (stringp value)
       (> (length value) 0)))

(defun ledger-import-account-ledger-name (account)
  "Return Ledger account name for ACCOUNT as known by your Ledger file."
  (nth 0 account))

(defun ledger-import-account-fetcher-id (account)
  "Return ACCOUNT identifier as known by the fetcher.
For example, this is the account ID that boobank uses."
  (nth 2 account))

(defun ledger-import-account-fid (account)
  "Return FID for ACCOUNT, or nil if none is necessary.
This can be useful for ledger-autosync if the OFX data does not provide any."
  (let ((fid (nth 3 account)))
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

;;;###autoload
(defun ledger-import-convert-ofx-to-ledger (account in-buffer &optional callback ledger-file)
  "Convert ofx data for ACCOUNT in IN-BUFFER to Ledger format.
Display result in `ledger-import-buffer' and execute CALLBACK when done.

`ledger-import-autosync-command' is used to do the convertion.

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

;;;###autoload
(defun ledger-import-fetch-boobank (fetcher-account &optional callback retry)
  "Use boobank to fetch OFX data for FETCHER-ACCOUNT, a string.
When done, execute CALLBACK with buffer containing OFX data.

RETRY is a number (default 3) indicating the number of times
boobank is executed if it fails.  This is because boobank tends
to fail often and restarting usually solves the problem."
  (interactive (list (ledger-import-account-fetcher-id (ledger-import-choose-account)) #'ledger-import-pop-to-buffer))
  (let ((retry (or retry 3))
        (buffer (generate-new-buffer (format "*ledger-import-%s*" fetcher-account)))
        (error-buffer (generate-new-buffer (format "*ledger-import-%s <stderr>*" fetcher-account)))
        (command `(,@ledger-import-boobank-command
                   "--formatter=ofx"
                   "history"
                   ,fetcher-account
                   ,ledger-import-boobank-import-from-date)))
    (with-current-buffer buffer
      (message "Starting boobank for %s" fetcher-account)
      (make-process
       :name (format "boobank %s" fetcher-account)
       :buffer buffer
       :stderr error-buffer
       :command command
       :sentinel (lambda (_process event)
                   (when (string= event "finished\n")
                     (if (not (with-current-buffer error-buffer (= (point-min) (point-max))))
                         (ledger-import--fetch-boobank-error retry fetcher-account callback error-buffer)
                       (kill-buffer error-buffer)
                       (with-current-buffer buffer (run-hooks 'ledger-import-fetched-hook))
                       (when callback (funcall callback buffer))))
                   (when (string-prefix-p "exited abnormally" event)
                     (ledger-import--fetch-boobank-error retry fetcher-account callback error-buffer)))))))

(defun ledger-import--fetch-boobank-error (retry account callback error-buffer)
  "Throw an error if RETRY is 0 or try starting boobank again.

ACCOUNT and CALLBACK are the same as in `ledger-import-fetch-boobank'.

ERROR-BUFFER is a buffer containing an error message explaining the problem."
  (if (>= retry 0)
      (ledger-import-fetch-boobank account callback (1- retry))
    (pop-to-buffer-same-window error-buffer)
    (error "There was a problem with boobank while importing %s" account)))

(defun ledger-import-ofx-rewrite ()
  "Apply `ledger-import-ofx-rewrite-rules' to current buffer.
The current buffer should be in the OFX format."
  (save-match-data
    (dolist (pair ledger-import-ofx-rewrite-rules)
      (goto-char (point-min))
      (while (re-search-forward (car pair) nil t)
        (replace-match (cdr pair) t)))))

(defun ledger-import-account (account &optional callback ledger-file)
  "Fetch and convert transactions of ACCOUNT.
Write the result in `ledger-import-buffer' and execute CALLBACK when done.

If LEDGER-FILE is non nil, use transactions from this file to
guess related account names."
  (interactive
   (list (ledger-import-choose-account)
         (lambda () (ledger-import-pop-to-buffer))))
  (ledger-import-fetch-boobank
   (ledger-import-account-fetcher-id account)
   (lambda (ofx-buffer)
     (ledger-import-convert-ofx-to-ledger
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
    (ledger-import-account
     (car accounts)
     (lambda ()
       (ledger-import--accounts (cdr accounts) callback ledger-file))
     ledger-file)))

;;;###autoload
(defun ledger-import-all-accounts (&optional ledger-file)
  "Fetch transactions from all accounts and convert to Ledger format.
Accounts are listed `ledger-import-accounts'.

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
