;;; test-helper.el --- ERT for ledger-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 John Wiegley <johnw AT gnu DOT org>

;; Author: Thierry <thdox AT free DOT fr>
;; Keywords: languages
;; Homepage: https://github.com/ledger/ledger-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 51
;; Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;  ERT test helpers for ledger-mode

;;; Code:
(require 'ledger-mode)
(require 'ert)
(require 'cl-lib)
(require 'cus-edit)

(defvar demo-ledger
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name
      "input/demo.ledger"
      (file-name-directory load-file-name)))
    (buffer-string)))


(defun ledger-tests-reset-custom-values (group)
  "Reset custom variables from GROUP to standard value."
  (let ((members (custom-group-members group nil)))
    (dolist (member members)
      (cond ((eq (cadr member) 'custom-group)
             (ledger-tests-reset-custom-values (car member)))
            ((eq (cadr member) 'custom-variable)
             (custom-reevaluate-setting (car member)))))))

(defun ledger-tests-with-simulated-input-1 (keys f)
  "Call F with no arguments, providing KEYS as simulated keyboard input."
  (let* ((overriding-terminal-local-map
          (if (keymapp overriding-terminal-local-map)
              (copy-keymap overriding-terminal-local-map)
            (make-sparse-keymap)))
         ;; Pick an arbitrary key that is likely to be unbound, and should
         ;; definitely be by default
         (next-action-key (kbd "C-M-S-<f19>"))
         (return-tag (make-symbol "return"))
         (error-tag (make-symbol "error"))
         (actions (list
                   (lambda () (throw return-tag (funcall f)))
                   (lambda () (execute-kbd-macro (kbd keys)))
                   (lambda ()
                     (error "Did not return from function after providing input %S" keys)))))

    (cl-assert (null (key-binding next-action-key)))

    (define-key overriding-terminal-local-map next-action-key
                (lambda ()
                  (interactive)
                  (condition-case e
                      (funcall (pop actions))
                    (error (throw error-tag e)))))

    (catch return-tag
      (let ((error
             (catch error-tag
               (execute-kbd-macro (cl-loop repeat (length actions)
                                           vconcat next-action-key)))))
        (signal (car error) (cdr error))))))

(defmacro ledger-tests-with-simulated-input (keys &rest body)
  ;; implementation inspired by
  ;; https://github.com/DarwinAwardWinner/with-simulated-input/
  "Execute BODY, providing KEYS as simulated keyboard input.

Error if BODY does not return after the input has been
provided (e.g., if BODY is still inside a minibuffer prompt)."
  (declare (indent 1) (debug t))
  `(ledger-tests-with-simulated-input-1 ,keys (lambda () ,@body)))

(defun ledger-tests-with-temp-file-1 (contents body)
  (let* ((temp-file (make-temp-file "ledger-tests-"))
         (ledger-buffer (find-file-noselect temp-file))
         (ledger-init-file-name nil))
    (unwind-protect
        (with-current-buffer ledger-buffer
          (switch-to-buffer ledger-buffer) ; this selects window
          (ledger-mode)
          (insert contents)
          (goto-char (point-min))
          (funcall body))
      (when (buffer-live-p ledger-buffer)
        (with-current-buffer ledger-buffer
          (set-buffer-modified-p nil)
          (kill-buffer)))
      (ledger-tests-reset-custom-values 'ledger)
      (delete-file temp-file))))

(defmacro ledger-tests-with-temp-file (contents &rest body)
  ;; from python-tests-with-temp-file
  "Create a `ledger-mode' enabled file with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(ledger-tests-with-temp-file-1 ,contents (lambda () ,@body)))

(defmacro ledger-tests-with-time-zone (tz &rest body)
  "Temporarily set local time zone to TZ while executing BODY."
  (declare (indent 1) (debug t))
  (let ((old-tz (make-symbol "old-tz")))
    `(let ((,old-tz (getenv "TZ")))
       (setenv "TZ" ,tz)
       (unwind-protect
           (progn ,@body)
         (setenv "TZ" ,old-tz)))))


(defun ledger-test-visible-buffer-string ()
  "Same as `buffer-string', but excludes invisible text."
  (ledger-test-visible-buffer-substring (point-min) (point-max)))


(defun ledger-test-visible-buffer-substring (start end)
  "Same as `buffer-substring', but excludes invisible text.
The two arguments START and END are character positions."
  (let (str)
    (while (< start end)
      (let ((next-pos (next-char-property-change start end)))
        (when (not (invisible-p start))
          (setq str (concat str (buffer-substring start next-pos))))
        (setq start next-pos)))
    str))


;; --------------------------------------------------------------------
;; Font lock test helpers
;; --------------------------------------------------------------------

(defun ledger-test-fontify-string (str)
  "Fontify `STR' in ledger mode."
  (let (ledger-init-file-name)
    (with-temp-buffer
      (ledger-mode)
      (insert str)
      (font-lock-ensure)
      ;; This second call appears to prevent `ledger-fontify/test-017' from
      ;; nondeterministically failing on Emacs 25.1.  When ledger-mode no longer
      ;; supports that Emacs version, this second call can likely be removed.
      (font-lock-ensure)
      (buffer-string))))


(defun ledger-test-face-groups (fontified)
  "Group a FONTIFIED string by face.
Return a list of substrings each followed by its face."
  (cl-loop for start = 0 then end
           while start
           for end   = (next-single-property-change start 'face fontified)
           for prop  = (get-text-property start 'face fontified)
           for text  = (substring-no-properties fontified start end)
           if prop
           append (list text prop)))

(defun ledger-test-group-str-by-face (str)
  "Fontify STR in ledger mode and group it by face.
Return a list of substrings each followed by its face."
  (ledger-test-face-groups (ledger-test-fontify-string str)))


(defun ledger-test-font-lock (source face-groups)
  "Test that `SOURCE' fontifies to the expected `FACE-GROUPS'."
  (should (equal (ledger-test-group-str-by-face source)
                 face-groups)))


(provide 'test-helper)

;;; test-helper.el ends here
