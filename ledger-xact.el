;;; ledger-xact.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.


;;; Commentary:
;; Utilities for running ledger synchronously.

;;; Code:

(require 'eshell)
(require 'ledger-regex)
(require 'ledger-navigate)
(require 'ledger-exec)
(require 'ledger-post)
(declare-function ledger-read-date "ledger-mode" (prompt))
(declare-function ledger-mode "ledger-mode" ())

;; TODO: This file depends on code in ledger-mode.el, which depends on this.

(defcustom ledger-highlight-xact-under-point t
  "If t highlight xact under point."
  :type 'boolean
  :group 'ledger)

(defcustom ledger-add-transaction-prompt-for-text t
  "When non-nil, use ledger xact to format transaction.
When nil, `ledger-add-transaction' will not prompt twice."
  :type 'boolean
  :package-version '(ledger-mode . "4.0.1")
  :group 'ledger)

(defcustom ledger-add-transaction-idle-preview t
  "When non-nil, a live preview of the to-be-added transaction is shown.
Requires `ledger-add-transaction-prompt-for-text' to be non-nil."
  :type '(choice (const :tag "Do not preview" nil)
                 (const :tag "Preview when idle" t)
                 (number :tag "Preview with custom delay"))
  :package-version '(ledger-mode . "4.1")
  :group 'ledger)

(defvar-local ledger-xact-highlight-overlay (list))

(defun ledger-highlight-make-overlay ()
  (let ((ovl (make-overlay 1 1)))
    (overlay-put ovl 'font-lock-face 'ledger-font-xact-highlight-face)
    (overlay-put ovl 'priority '(nil . 99))
    ovl))

(defun ledger-highlight-xact-under-point ()
  "Move the highlight overlay to the current transaction."
  (when ledger-highlight-xact-under-point
    (unless ledger-xact-highlight-overlay
      (setq ledger-xact-highlight-overlay (ledger-highlight-make-overlay)))
    (let ((exts (ledger-navigate-find-element-extents (point))))
      (let ((b (car exts))
            (e (cadr exts))
            (p (point)))
        (if (and (> (- e b) 1)            ; not an empty line
                 (<= p e) (>= p b)        ; point is within the boundaries
                 (not (region-active-p))) ; no active region
            (move-overlay ledger-xact-highlight-overlay b (+ 1 e))
          (move-overlay ledger-xact-highlight-overlay 1 1))))))

(defun ledger-highlight--before-revert ()
  "Clean up highlighting overlay before reverting buffer."
  (when ledger-xact-highlight-overlay
    (delete-overlay ledger-xact-highlight-overlay)))

(defun ledger-xact-context ()
  "Return the context of the transaction containing point or nil."
  (let ((i 0))
    (while (eq (ledger-context-line-type (ledger-context-other-line i)) 'acct-transaction)
      (setq i (- i 1)))
    (let ((context-info (ledger-context-other-line i)))
      (when (eq (ledger-context-line-type context-info) 'xact)
        context-info))))

(defun ledger-xact-payee ()
  "Return the payee of the transaction containing point or nil."
  (when-let* ((xact-context (ledger-xact-context)))
    (ledger-context-field-value xact-context 'payee)))

(defun ledger-xact-date ()
  "Return the date of the transaction containing point or nil."
  (when-let* ((xact-context (ledger-xact-context)))
    (ledger-context-field-value xact-context 'date)))

(defun ledger-xact-find-slot (moment)
  "Find the right place in the buffer for a transaction at MOMENT.
MOMENT is an encoded date"
  (let (last-xact-start)
    (catch 'found
      (ledger-xact-iterate-transactions
       (lambda (start date _mark _desc)
         (setq last-xact-start start)
         (when (time-less-p moment date)
           (throw 'found t)))))
    ;; If we are inserting at the end of the buffer, insert an extra newline
    (when (and (eobp) last-xact-start)
      (let ((end (cadr (ledger-navigate-find-xact-extents last-xact-start))))
        (goto-char end)
        (insert "\n")
        (forward-line)))))

(defun ledger-xact-iterate-transactions (callback)
  "Iterate through each transaction call CALLBACK for each."
  (goto-char (point-min))
  (let* ((now (current-time))
         (current-year (nth 5 (decode-time now))))
    (while (not (eobp))
      (when (looking-at ledger-iterate-regexp)
        (if-let* ((year (match-string 1)))
            (setq current-year (string-to-number year)) ;a Y directive was found
          (let ((start (match-beginning 0))
                (year (match-string (+ ledger-regex-iterate-group-actual-date 1)))
                (month (string-to-number (match-string (+ ledger-regex-iterate-group-actual-date 2))))
                (day (string-to-number (match-string (+ ledger-regex-iterate-group-actual-date 3))))
                (state (match-string ledger-regex-iterate-group-state))
                (payee (match-string ledger-regex-iterate-group-payee)))
            (if (and year (> (length year) 0))
                (setq year (string-to-number year)))
            (funcall callback start
                     (encode-time 0 0 0 day month
                                  (or year current-year))
                     state payee))))
      (forward-line))))

(defcustom ledger-copy-transaction-insert-blank-line-after nil
  "When non-nil, insert a blank line after `ledger-copy-transaction-at-point'."
  :type 'boolean
  :group 'ledger)

(defun ledger-copy-transaction-at-point (date)
  "Ask for a new DATE and copy the transaction under point to that date.
Leave point on the first amount, if any, otherwise the first account."
  (interactive (list (ledger-read-date "Copy to date: ")))
  (let* ((extents (ledger-navigate-find-xact-extents (point)))
         (transaction (buffer-substring-no-properties (car extents) (cadr extents)))
         (encoded-date (ledger-parse-iso-date date)))
    (push-mark)
    (ledger-xact-find-slot encoded-date)
    (insert transaction
            (if (and ledger-copy-transaction-insert-blank-line-after (not (eobp)))
                "\n\n"
              "\n"))
    (beginning-of-line -1)
    (ledger-navigate-beginning-of-xact)
    (let ((end (save-excursion (ledger-navigate-end-of-xact) (point))))
      (re-search-forward ledger-iso-date-regexp)
      (replace-match date)
      (if (ledger-next-amount end)
          (progn
            (re-search-forward "[-0-9]")
            (goto-char (match-beginning 0)))
        (ledger-next-account end)))))

(defun ledger-delete-current-transaction (pos)
  "Delete the transaction surrounding POS."
  (interactive "d")
  (let ((bounds (ledger-navigate-find-xact-extents pos)))
    (delete-region (car bounds) (cadr bounds)))
  (delete-blank-lines))

(defvar ledger-add-transaction-last-date nil
  "Last date entered using `ledger-read-transaction'.")

(defvar ledger-xact--preview-buffer-name "*ledger xact preview*")
(defvar-local ledger-xact--preview-timer nil)
(defvar-local ledger-xact--date nil
  "In a minibuffer for the transaction text, the transaction date.")
(defvar-local ledger-xact--ledger-buf-file nil
  "In a minibuffer for the transaction text, the input file.

The original ledger buffer is written to this temporary file so it can
be read by ledger.  This is quite a bit faster than passing in the input
via `process-send-region'.")

(defun ledger-xact--preview (date args)
  "Run \"ledger xact\" with DATE and ARGS and display the output.

`ledger-xact--ledger-buf-file' is used as input to \"ledger xact\".

Return the window displaying the output buffer, or nil if it was not
displayed."
  (let ((preview-buf
         (or (get-buffer ledger-xact--preview-buffer-name)
             (with-current-buffer (get-buffer-create ledger-xact--preview-buffer-name)
               ;; Enable `ledger-mode' just for syntax highlighting.  Skip all minor
               ;; modes except for `font-lock-mode'.
               (delay-mode-hooks (ledger-mode))
               (font-lock-mode)
               (setq buffer-read-only t)
               (set-buffer-modified-p nil)
               (current-buffer))))
        (input-file ledger-xact--ledger-buf-file)
        window)
    (with-current-buffer preview-buf
      (with-silent-modifications
        ;; Don't use `ledger-exec-ledger' because it pops up any error output in
        ;; a separate buffer.  For this use case, it is preferable to display
        ;; the error in the preview buffer instead.
        ;;
        ;; Also, it uses `call-process-region', which behaves poorly with
        ;; `while-no-input': if two input events arrive quickly, they may both
        ;; be lost.  (Try evaluating (while-no-input (call-process "sleep" nil
        ;; nil nil "10")) and then typing "asdf").
        ;;
        ;; Sadly, using `process-send-region' is quite a bit slower than
        ;; `call-process-region'.
        ;;
        ;; TODO: Could we speed up the previews slightly by calling "ledger -f
        ;; -" (even before the user has begun typing any input) and merely
        ;; inputting "xact" commands at the REPL when the input changes?
        (erase-buffer)
        (while-no-input
          (unwind-protect
              (let ((proc (make-process
                           :name "ledger-xact-preview"
                           :buffer preview-buf
                           :command (append (list ledger-binary-path
                                                  "-f" input-file
                                                  "xact" date)
                                            args)
                           :noquery t
                           :connection-type 'pipe
                           :sentinel #'ignore)))
                (process-send-eof proc)
                (while (accept-process-output proc)))
            (when (get-buffer-process preview-buf)
              (delete-process preview-buf))))
        (ledger-post-align-postings (point-min) (point-max))))
    (setq window
          (display-buffer preview-buf
                          '((display-buffer-reuse-window display-buffer-at-bottom)
                            (window-height . fit-window-to-buffer))))
    ;; modeled after `internal-temp-output-buffer-show'
    (when window
      (setq minibuffer-scroll-window window)
      (set-window-hscroll window 0)
      (set-window-start window (point-min) t)
      (set-window-point window (point-min)))
    window))

(defun ledger-xact--preview-timer (minibuffer)
  "Preview the ledger xact output from MINIBUFFER's current contents."
  (setq ledger-xact--preview-timer nil)
  ;; TODO: It would be more correct to use `minibufferp' and pass a non-nil LIVE
  ;; argument, but that feature isn't available until Emacs 28.3.
  (when (and (buffer-live-p minibuffer)
             (eq minibuffer (window-buffer (active-minibuffer-window))))
    (with-current-buffer minibuffer
      (let ((date ledger-xact--date))
        (when-let* ((args (ledger-parse-transaction-text (minibuffer-contents))))
          (ledger-xact--preview date args))))))

(defun ledger-xact--after-change-function (_beg _end _len)
  "Added to `after-change-functions' in transaction-reading minibuffers."
  (unless ledger-xact--preview-timer
    (setq ledger-xact--preview-timer
          (run-with-idle-timer
           (if (numberp ledger-add-transaction-idle-preview)
               ledger-add-transaction-idle-preview
             0.1)
           nil #'ledger-xact--preview-timer (current-buffer)))))

(defun ledger-xact--hide-preview-window ()
  "Similar to `minibuffer-restore-windows', for transaction-reading minibuffers."
  ;; This variable was introduced in Emacs 28.1.  The default, matching the
  ;; behavior in previous versions of Emacs, is equivalent to non-nil.  We only
  ;; want to delete the window if the default window configuration restore logic
  ;; wouldn't have.
  (when (and (boundp 'read-minibuffer-restore-windows)
             (not read-minibuffer-restore-windows))
    (when-let* ((window (get-buffer-window ledger-xact--preview-buffer-name)))
      (delete-window window))))

(defun ledger-xact--delete-preview-temp-file ()
  (when ledger-xact--ledger-buf-file
    (delete-file ledger-xact--ledger-buf-file)))

(defun ledger-read-transaction-text (date)
  "Read the text of a transaction with date DATE.

The ledger buffer should be current when this function is called, since
it will be used as input for \"ledger xact\" for the sake of previewing
output."
  (let ((ledger-buf (current-buffer))
        (ledger-buf-dir default-directory))
    (minibuffer-with-setup-hook
        (lambda ()
          (when ledger-add-transaction-idle-preview
            (setq ledger-xact--date date
                  ledger-xact--ledger-buf-file
                  (let* ((temporary-file-directory ledger-buf-dir)
                         (filename (make-temp-file "ldg-xact-preview" nil ".ldg")))
                    (with-current-buffer ledger-buf
                      (save-restriction
                        (widen)
                        (write-region nil nil filename nil 'nomessage)))
                    filename))
            (add-hook 'after-change-functions #'ledger-xact--after-change-function nil t)
            (add-hook 'minibuffer-exit-hook #'ledger-xact--hide-preview-window nil t)
            (add-hook 'minibuffer-exit-hook #'ledger-xact--delete-preview-temp-file nil t)))
      (read-string (concat "xact " date ": ") nil 'ledger-minibuffer-history))))

(defun ledger-read-transaction ()
  "Read the text of a transaction, which is at least the current date."
  (let ((date (ledger-read-date "Date: ")))
    (concat date " "
            (when ledger-add-transaction-prompt-for-text
              (ledger-read-transaction-text date)))))

(defun ledger-parse-iso-date (date)
  "Try to parse DATE using `ledger-iso-date-regexp' and return a time value or nil."
  (when (string-match ledger-iso-date-regexp date)
    (encode-time 0 0 0 (string-to-number (match-string 4 date))
                 (string-to-number (match-string 3 date))
                 (string-to-number (match-string 2 date)))))

(defun ledger-parse-transaction-text (transaction-text)
  "Parse TRANSACTION-TEXT as a date and maybe some arguments.

Return (DATE . ARGS), a list of strings."
  ;; TODO: This whole function could just be replaced with
  ;; `split-string-shell-command' when the minimum supported Emacs version is
  ;; Emacs 28.
  (with-temp-buffer
    (insert transaction-text)
    (mapcar #'eval (eshell-parse-arguments (point-min) (point-max)))))

(defun ledger-add-transaction (transaction-text &optional insert-at-point)
  "Use ledger xact TRANSACTION-TEXT to add a transaction to the buffer.

If INSERT-AT-POINT is non-nil insert the transaction there, otherwise
call `ledger-xact-find-slot' to insert it at the correct chronological
place in the buffer.

Interactively, the date is requested via `ledger-read-date' and the
\\[universal-argument] enables INSERT-AT-POINT."
  (interactive (list (ledger-read-transaction) current-prefix-arg))
  (let* ((args (ledger-parse-transaction-text transaction-text))
         (date (pop args))
         (ledger-buf (current-buffer))
         (separator "\n"))
    (unless insert-at-point
      (let* ((parsed-date (ledger-parse-iso-date date)))
        (setq ledger-add-transaction-last-date parsed-date)
        (push-mark)
        ;; TODO: what about when it can't be parsed?
        (ledger-xact-find-slot (or parsed-date date))
        (when (looking-at-p "\n*\\'")
          (setq separator ""))))
    (if args
        (save-excursion
          (insert
           (with-temp-buffer
             (apply #'ledger-exec-ledger ledger-buf (current-buffer) "xact" date args)
             (goto-char (point-min))
             (ledger-post-align-postings (point-min) (point-max))
             (buffer-string))
           separator))
      (insert date " ")
      (save-excursion (insert "\n" separator)))))

(provide 'ledger-xact)

;;; ledger-xact.el ends here
