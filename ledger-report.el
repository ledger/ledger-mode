;;; ledger-report.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

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
;;  Provide facilities for running and saving reports in Emacs

;;; Code:

(require 'ledger-xact)
(require 'ledger-navigate)
(declare-function ledger-read-string-with-default "ledger-mode" (prompt default))
(declare-function ledger-read-account-with-prompt "ledger-mode" (prompt))

(require 'easymenu)
(require 'ansi-color)
(require 'font-lock)

(defvar ledger-buf)

(defgroup ledger-report nil
  "Customization option for the Report buffer"
  :group 'ledger)

(defcustom ledger-reports
  '(("bal" "%(binary) -f %(ledger-file) bal")
    ("reg" "%(binary) -f %(ledger-file) reg")
    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
    ("account" "%(binary) -f %(ledger-file) reg %(account)"))
  "Definition of reports to run.

Each element has the form (NAME CMDLINE).  The command line can
contain format specifiers that are replaced with context sensitive
information.  Format specifiers have the format '%(<name>)' where
<name> is an identifier for the information to be replaced.  The
`ledger-report-format-specifiers' alist variable contains a mapping
from format specifier identifier to a Lisp function that implements
the substitution.  See the documentation of the individual functions
in that variable for more information on the behavior of each
specifier."
  :type '(repeat (list (string :tag "Report Name")
                       (string :tag "Command Line")))
  :group 'ledger-report)

(defcustom ledger-report-format-specifiers
  '(("ledger-file" . ledger-report-ledger-file-format-specifier)
    ("binary" . ledger-report-binary-format-specifier)
    ("payee" . ledger-report-payee-format-specifier)
    ("account" . ledger-report-account-format-specifier)
    ("month" . ledger-report-month-format-specifier)
    ("tagname" . ledger-report-tagname-format-specifier)
    ("tagvalue" . ledger-report-tagvalue-format-specifier))
  "An alist mapping ledger report format specifiers to implementing functions.

The function is called with no parameters and expected to return
a string, or a list of strings, that should replace the format specifier.
Single strings are quoted with `shell-quote-argument'; lists of strings are
simply concatenated (no quoting)."
  :type 'alist
  :group 'ledger-report)

(defcustom ledger-report-auto-refresh t
  "If non-nil, automatically rerun the report when the ledger buffer is saved."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-auto-refresh-sticky-cursor nil
  "If non-nil, place cursor at same relative position as it was before auto-refresh."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-links-in-register t
  "If non-nil, link entries in \"register\" reports to entries in the ledger buffer."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-use-native-highlighting t
  "When non-nil, use ledger's native highlighting in reports."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-auto-width t
  "When non-nil, tell ledger about the width of the report window."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-use-header-line nil
  "If non-nil, indicate report name/command in the `header-line'.
The report name/command won't be printed in the buffer.  See
`ledger-report-header-line-fn' for how to customize the
information reported."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-header-line-fn #'ledger-report--header-function
  "Evaluate this function in the `header-line' of the report buffer.
`ledger-report-use-header-line' must be non-nil for this to have any effect."
  :type 'function
  :group 'ledger-report)

(defcustom ledger-report-resize-window t
  "If non-nil, resize the report window.
Calls `shrink-window-if-larger-than-buffer'."
  :type 'boolean
  :group 'ledger-report)

(defcustom ledger-report-use-strict nil
  "Should Ledger-mode pass --strict as a command line parameter
when running reports?"
  :type 'boolean
  :group 'ledger-report)

(defvar ledger-report-buffer-name "*Ledger Report*")

(defvar ledger-report-name nil)
(defvar ledger-report-cmd nil)
(defvar ledger-report-name-prompt-history nil)
(defvar ledger-report-cmd-prompt-history nil)
(defvar ledger-original-window-cfg nil)
(defvar ledger-report-saved nil)
(defvar ledger-minibuffer-history nil)
(defvar ledger-report-mode-abbrev-table)
(defvar ledger-report-current-month nil)

(defvar ledger-report-is-reversed nil)
(defvar ledger-report-cursor-line-number nil)

(defun ledger-report-reverse-report ()
  "Reverse the order of the report."
  (interactive)
  (ledger-report-reverse-lines)
  (setq ledger-report-is-reversed (not ledger-report-is-reversed)))

(defun ledger-report-reverse-lines ()
  "Reverse the lines in the ledger report buffer."
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (unless ledger-report-use-header-line
      (forward-paragraph)
      (forward-line))
    (save-excursion
      (reverse-region (point) (point-max)))))

(defun ledger-report-maybe-shrink-window ()
  "Shrink window if `ledger-report-resize-window' is non-nil."
  (when ledger-report-resize-window
    (shrink-window-if-larger-than-buffer)))

(defvar ledger-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [? ] #'scroll-up)
    (define-key map [backspace] #'scroll-down)
    (define-key map [?r] #'ledger-report-redo)
    (define-key map [(shift ?r)] #'ledger-report-reverse-report)
    (define-key map [?s] #'ledger-report-save)
    (define-key map [(shift ?s)] #'ledger-report-select-report)
    (define-key map [?e] #'ledger-report-edit-report)
    (define-key map [( shift ?e)] #'ledger-report-edit-reports)
    (define-key map [?q] #'ledger-report-quit)
    (define-key map [?g] #'ledger-report-redo)
    (define-key map [(control ?c) (control ?l) (control ?r)]
      #'ledger-report-redo)
    (define-key map [(control ?c) (control ?l) (control ?S)]
      #'ledger-report-save)
    (define-key map [(control ?c) (control ?l) (control ?e)]
      #'ledger-report-edit-report)
    (define-key map (kbd "M-p") #'ledger-report-previous-month)
    (define-key map (kbd "M-n") #'ledger-report-next-month)
    map)
  "Keymap for `ledger-report-mode'.")

(easy-menu-define ledger-report-mode-menu ledger-report-mode-map
  "Ledger report menu"
  '("Reports"
    ["Select Report" ledger-report-select-report]
    ["Save Report" ledger-report-save]
    ["Edit Current Report" ledger-report-edit-report]
    ["Edit All Reports" ledger-report-edit-reports]
    ["Re-run Report" ledger-report-redo]
    "---"
    ["Reverse report order" ledger-report-reverse-report]
    "---"
    ["Scroll Up" scroll-up]
    ["Visit Source" ledger-report-visit-source]
    ["Scroll Down" scroll-down]
    "---"
    ["Quit" ledger-report-quit]
    ))

(define-derived-mode ledger-report-mode text-mode "Ledger-Report"
  "A mode for viewing ledger reports.")

(defconst ledger-report--extra-args-marker "[[ledger-mode-flags]]")

(defun ledger-report-binary-format-specifier ()
  "Return the path to ledger, plus a marker for extra arguments."
  (list (shell-quote-argument ledger-binary-path)
        ledger-report--extra-args-marker))

(defun ledger-report-tagname-format-specifier ()
  "Return a valid meta-data tag name."
  ;; It is intended completion should be available on existing tag
  ;; names, but it remains to be implemented.
  (ledger-read-string-with-default "Tag Name: " nil))

(defun ledger-report-tagvalue-format-specifier ()
  "Return a valid meta-data tag name."
  ;; It is intended completion should be available on existing tag
  ;; values, but it remains to be implemented.
  (ledger-read-string-with-default "Tag Value: " nil))

(defun ledger-report-read-name ()
  "Read the name of a ledger report to use, with completion.

The empty string and unknown names are allowed."
  (completing-read "Report name: "
                   ledger-reports nil nil nil
                   'ledger-report-name-prompt-history nil))

(defun ledger-report (report-name edit)
  "Run a user-specified report from `ledger-reports'.

Prompts the user for the REPORT-NAME of the report to run or
EDIT.  If no name is entered, the user will be prompted for a
command line to run.  The command line specified or associated
with the selected report name is run and the output is made
available in another buffer for viewing.  If a prefix argument is
given and the user selects a valid report name, the user is
prompted with the corresponding command line for editing before
the command is run.

The output buffer will be in `ledger-report-mode', which defines
commands for saving a new named report based on the command line
used to generate the buffer, navigating the buffer, etc."
  (interactive
   (progn
     (when (and (buffer-modified-p)
                (y-or-n-p "Buffer modified, save it? "))
       (save-buffer))
     (let ((rname (ledger-report-read-name))
           (edit (not (null current-prefix-arg))))
       (list rname edit))))
  (let ((buf (find-file-noselect (ledger-master-file)))
        (rbuf (get-buffer ledger-report-buffer-name))
        (wcfg (current-window-configuration)))
    (if rbuf
        (kill-buffer rbuf))
    (with-current-buffer
        (pop-to-buffer (get-buffer-create ledger-report-buffer-name))
      (ledger-report-mode)
      (set (make-local-variable 'ledger-report-saved) nil)
      (set (make-local-variable 'ledger-buf) buf)
      (set (make-local-variable 'ledger-report-name) report-name)
      (set (make-local-variable 'ledger-original-window-cfg) wcfg)
      (set (make-local-variable 'ledger-report-is-reversed) nil)
      (set (make-local-variable 'ledger-report-current-month) nil)
      (ledger-do-report (ledger-report-cmd report-name edit))
      (ledger-report-maybe-shrink-window)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (message "q to quit; r to redo; e to edit; k to kill; s to save; SPC and DEL to scroll"))))

(defun ledger-report--header-function ()
  "Compute the string to be used as the header in the `ledger-report' buffer."
  (format "Ledger Report: %s -- Buffer: %s -- Command: %s"
          (propertize ledger-report-name 'face 'font-lock-constant-face)
          (propertize (buffer-name ledger-buf) 'face 'font-lock-string-face)
          (propertize ledger-report-cmd 'face 'font-lock-comment-face)))

(defun ledger-report-string-empty-p (s)
  "Check S for the empty string."
  (string-equal "" s))

(defun ledger-report-name-exists (name)
  "Check to see if the given report NAME exists.

   If name exists, returns the object naming the report,
   otherwise returns nil."
  (unless (ledger-report-string-empty-p name)
    (car (assoc name ledger-reports))))

(defun ledger-reports-add (name cmd)
  "Add a new report NAME and CMD to `ledger-reports'."
  (setq ledger-reports (cons (list name cmd) ledger-reports)))

(defun ledger-reports-custom-save ()
  "Save the `ledger-reports' variable using the customize framework."
  (customize-save-variable 'ledger-reports ledger-reports))

(defun ledger-report-read-command (report-cmd)
  "Read the command line to create a report from REPORT-CMD."
  (read-from-minibuffer "Report command line: "
                        (if (null report-cmd) "ledger " report-cmd)
                        nil nil 'ledger-report-cmd-prompt-history))

(defun ledger-report-ledger-file-format-specifier ()
  "Substitute the full path to master or current ledger file.

   The master file name is determined by the variable `ledger-master-file'
   buffer-local variable which can be set using file variables.
   If it is set, it is used, otherwise the current buffer file is
   used."
  (ledger-master-file))

;; General helper functions

(defvar-local ledger-master-file nil
  "The master file for the current buffer.
See documentation for the function `ledger-master-file'")

(defun ledger-master-file ()
  "Return the master file for a ledger file.

   The master file is either the file for the current ledger buffer or the
   file specified by the buffer-local variable `ledger-master-file'.  Typically
   this variable would be set in a file local variable comment block at the
   end of a ledger file which is included in some other file."
  (if ledger-master-file
      (expand-file-name ledger-master-file)
    (buffer-file-name)))

(defun ledger-report-payee-format-specifier ()
  "Substitute a payee name.

   The user is prompted to enter a payee and that is substitued.  If
   point is in an xact, the payee for that xact is used as the
   default."
  ;; It is intended completion should be available on existing
  ;; payees, but the list of possible completions needs to be
  ;; developed to allow this.
  (ledger-read-string-with-default "Payee" (regexp-quote (ledger-xact-payee))))

(defun ledger-report-account-format-specifier ()
  "Substitute an account name.

   The user is prompted to enter an account name, which can be any
   regular expression identifying an account.  If point is on an account
   posting line for an xact, the full account name on that line is
   the default."
  (ledger-read-account-with-prompt "Account"))

(defun ledger-report--current-month ()
  "Return current month as (YEAR . MONTH-INDEX).

MONTH-INDEX ranges from 1 (January) to 12 (December) and YEAR is
a number."
  (let* ((time-parts (decode-time))
         (year (nth 5 time-parts))
         (month-index (nth 4 time-parts)))
    (cons year month-index)))

(defun ledger-report--shift-month (month shift)
  "Return (YEAR . NEW-MONTH) where NEW-MONTH is MONTH+SHIFT.

MONTH is of the form (YEAR . INDEX) where INDEX ranges from
1 (January) to 12 (December) and YEAR is a number."
  (let* ((year (car month))
         (new-month (+ (cdr month) shift)))
    (cons year new-month)))

(defun ledger-report-month-format-specifier ()
  "Substitute current month."
  (with-current-buffer (or ledger-report-buffer-name (current-buffer))
    (let* ((month (or ledger-report-current-month (ledger-report--current-month)))
           (year (car month))
           (month-index (cdr month)))
      (format "%s-%s" year month-index))))

(defun ledger-report-expand-format-specifiers (report-cmd)
  "Expand format specifiers in REPORT-CMD with thing under point."
  (save-match-data
    (let ((expanded-cmd report-cmd))
      (set-match-data (list 0 0))
      (while (string-match "%(\\([^)]*\\))" expanded-cmd
                           (if (> (length expanded-cmd) (match-end 0))
                               (match-end 0)
                             (1- (length expanded-cmd))))
        (let* ((specifier (match-string 1 expanded-cmd))
               (f (cdr (assoc specifier ledger-report-format-specifiers))))
          (if f
              (let* ((arg (save-match-data
                            (with-current-buffer ledger-buf
                              (funcall f))))
                     (quoted (if (listp arg)
                                 (mapconcat #'identity arg " ")
                               (shell-quote-argument arg))))
                (setq expanded-cmd (replace-match quoted t t expanded-cmd))))))
      expanded-cmd)))

(defun ledger-report--cmd-needs-links-p (cmd)
  "Check links should be added to the report produced by CMD."
  ;; --subtotal reports do not produce identifiable transactions, so
  ;; don't prepend location information for them
  (and (string-match "\\<reg\\(ister\\)?\\>" cmd)
       ledger-report-links-in-register
       (not (string-match "--subtotal" cmd))))

(defun ledger-report--compute-extra-args (report-cmd)
  "Compute extra args to add to REPORT-CMD."
  `(,@(when (ledger-report--cmd-needs-links-p report-cmd)
        '("--prepend-format=%(filename):%(beg_line):"))
    ,@(when ledger-report-auto-width
        `("--columns" ,(format "%d" (- (window-width) 1))))
    ,@(when ledger-report-use-native-highlighting
        '("--color" "--force-color"))
    ,@(when ledger-report-use-strict
        '("--strict"))))

(defun ledger-report-cmd (report-name edit)
  "Get the command line to run the report name REPORT-NAME.
Optionally EDIT the command."
  (let ((report-cmd (car (cdr (assoc report-name ledger-reports)))))
    ;; logic for substitution goes here
    (when (or (null report-cmd) edit)
      (setq report-cmd (ledger-report-read-command report-cmd))
      (setq ledger-report-saved nil)) ;; this is a new report, or edited report
    (setq report-cmd (ledger-report-expand-format-specifiers report-cmd))
    (set (make-local-variable 'ledger-report-cmd) report-cmd)
    (or (ledger-report-string-empty-p report-name)
        (ledger-report-name-exists report-name)
        (progn
          (ledger-reports-add report-name report-cmd)
          (ledger-reports-custom-save)))
    report-cmd))

(define-button-type 'ledger-report-register-entry
  'follow-link t
  'face nil ;; Otherwise make-text-button replaces Ledger's native highlighting
  'action (lambda (_button) (ledger-report-visit-source)))

(defun ledger-report--change-month (shift)
  "Rebuild report with transactions from current month + shift."
  (let* ((current-month (or ledger-report-current-month (ledger-report--current-month)))
         (previous-month (ledger-report--shift-month current-month shift)))
    (set (make-local-variable 'ledger-report-current-month) previous-month)
    (ledger-report-cmd ledger-report-name nil)
    (ledger-report-redo)))

(defun ledger-report--add-links ()
  "Replace file and line annotations with buttons."
  (while (re-search-forward "^\\(/[^:]+\\)?:\\([0-9]+\\)?:" nil t)
    (let ((file (match-string 1))
          (line (string-to-number (match-string 2))))
      (delete-region (match-beginning 0) (match-end 0))
      (when file
        (add-text-properties (line-beginning-position) (line-end-position)
                             (list 'ledger-source (cons file (save-window-excursion
                                                               (save-excursion
                                                                 (find-file file)
                                                                 (widen)
                                                                 (ledger-navigate-to-line line)
                                                                 (point-marker))))))
        (make-text-button
         (line-beginning-position) (line-end-position)
         'type 'ledger-report-register-entry
         'help-echo (format "mouse-2, RET: Visit %s:%d" file line))
        ;; Appending the face preserves Ledger's native highlighting
        (font-lock-append-text-property (line-beginning-position) (line-end-position)
                                'face 'ledger-font-report-clickable-face)
        (end-of-line)))))

(defun ledger-report--compute-header-line (cmd)
  "Call `ledger-report-header-line-fn' with `ledger-report-cmd' bound to CMD."
  (let ((ledger-report-cmd cmd))
    (funcall ledger-report-header-line-fn)))

(defun ledger-do-report (cmd)
  "Run a report command line CMD.
CMD may contain a (shell-quoted) version of
`ledger-report--extra-args-marker', wich will be replaced by
arguments returned by `ledger-report--compute-extra-args'."
  (goto-char (point-min))
  (let* ((marker ledger-report--extra-args-marker)
         (marker-re (concat " *" (regexp-quote marker)))
         (args (ledger-report--compute-extra-args cmd))
         (args-str (concat " " (mapconcat #'shell-quote-argument args " ")))
         (clean-cmd (replace-regexp-in-string marker-re "" cmd t t))
         (real-cmd (replace-regexp-in-string marker-re args-str cmd t t)))
    (setq header-line-format
          (and ledger-report-use-header-line
               `(:eval (ledger-report--compute-header-line ,clean-cmd))))
    (unless ledger-report-use-header-line
      (insert (format "Report: %s\n" ledger-report-name)
              (format "Command: %s\n" clean-cmd)
              (make-string (- (window-width) 1) ?=)
              "\n\n"))
    (let* ((report (shell-command-to-string real-cmd)))
      (when ledger-report-use-native-highlighting
        (setq report (ansi-color-apply report)))
      (save-excursion
        (insert report))
      (when (ledger-report--cmd-needs-links-p cmd)
        (save-excursion
          (ledger-report--add-links))))))

(defun ledger-report-visit-source ()
  "Visit the transaction under point in the report window."
  (interactive)
  (let* ((prop (get-text-property (point) 'ledger-source))
         (file (if prop (car prop)))
         (line-or-marker (if prop (cdr prop))))
    (when (and file line-or-marker)
      (find-file-other-window file)
      (widen)
      (if (markerp line-or-marker)
          (goto-char line-or-marker)
        (goto-char (point-min))
        (forward-line (1- line-or-marker))
        (re-search-backward "^[0-9]+")
        (beginning-of-line)
        (let ((start-of-txn (point)))
          (forward-paragraph)
          (narrow-to-region start-of-txn (point))
          (backward-paragraph))))))

(defun ledger-report-goto ()
  "Goto the ledger report buffer."
  (interactive)
  (let ((rbuf (get-buffer ledger-report-buffer-name)))
    (if (not rbuf)
        (error "There is no ledger report buffer"))
    (pop-to-buffer rbuf)
    (ledger-report-maybe-shrink-window)))

(defun ledger-report-redo ()
  "Redo the report in the current ledger report buffer."
  (interactive)
  (let ((cur-buf (current-buffer))
        (inhibit-read-only t))
    (if (and ledger-report-auto-refresh
             (or (string= (format-mode-line 'mode-name) "Ledger")
                 (string= (format-mode-line 'mode-name) "Ledger-Report"))
             (get-buffer ledger-report-buffer-name))
        (progn
          (pop-to-buffer (get-buffer ledger-report-buffer-name))
          (ledger-report-maybe-shrink-window)
          (setq ledger-report-cursor-line-number (line-number-at-pos))
          (erase-buffer)
          (ledger-do-report ledger-report-cmd)
          (if ledger-report-is-reversed (ledger-report-reverse-lines))
          (if ledger-report-auto-refresh-sticky-cursor (forward-line (- ledger-report-cursor-line-number 5)))
          (pop-to-buffer cur-buf)))))

(defun ledger-report-quit ()
  "Quit the ledger report buffer."
  (interactive)
  (ledger-report-goto)
  (set-window-configuration ledger-original-window-cfg)
  (kill-buffer (get-buffer ledger-report-buffer-name)))

(define-obsolete-function-alias 'ledger-report-kill #'ledger-report-quit)

(defun ledger-report-edit-reports ()
  "Edit the defined ledger reports."
  (interactive)
  (customize-variable 'ledger-reports))

(defun ledger-report-edit-report ()
  "Edit the current report command in the mini buffer and re-run the report."
  (interactive)
  (setq ledger-report-cmd (ledger-report-read-command ledger-report-cmd))
  (ledger-report-redo))

(defun ledger-report-select-report ()
  "Select and run one of the named reports."
  (interactive)
  (setq ledger-report-name (ledger-report-read-name)
        ledger-report-cmd (ledger-report-cmd ledger-report-name nil))
  (ledger-report-redo))

(defun ledger-report-read-new-name ()
  "Read the name for a new report from the minibuffer."
  (let ((name ""))
    (while (ledger-report-string-empty-p name)
      (setq name (read-from-minibuffer "Report name: " nil nil nil
                                       'ledger-report-name-prompt-history)))
    name))

(defun ledger-report-save ()
  "Save the current report command line as a named report."
  (interactive)
  (ledger-report-goto)
  (let (existing-name)
    (when (ledger-report-string-empty-p ledger-report-name)
      (setq ledger-report-name (ledger-report-read-new-name)))

    (if (setq existing-name (ledger-report-name-exists ledger-report-name))
        (cond ((y-or-n-p (format "Overwrite existing report named '%s'? "
                                 ledger-report-name))
               (if (string-equal
                    ledger-report-cmd
                    (car (cdr (assq existing-name ledger-reports))))
                   (message "Nothing to save. Current command is identical to existing saved one")
                 (progn
                   (setq ledger-reports
                         (assq-delete-all existing-name ledger-reports))
                   (ledger-reports-add ledger-report-name ledger-report-cmd)
                   (ledger-reports-custom-save))))
              (t
               (progn
                 (setq ledger-report-name (ledger-report-read-new-name))
                 (ledger-reports-add ledger-report-name ledger-report-cmd)
                 (ledger-reports-custom-save)))))))

(defun ledger-report-previous-month ()
  "Rebuild report with transactions from the previous month."
  (interactive)
  (ledger-report--change-month -1))

(defun ledger-report-next-month ()
  "Rebuild report with transactions from the next month."
  (interactive)
  (ledger-report--change-month 1))

(provide 'ledger-report)

;;; ledger-report.el ends here
