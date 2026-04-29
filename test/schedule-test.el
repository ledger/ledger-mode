;;; schedule-test.el --- ERT for ledger-schedule  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2025 John Wiegley <johnw AT gnu DOT org>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.

;;; Commentary:
;;  Regression tests for ledger-schedule

;;; Code:
(require 'test-helper)
(require 'ledger-schedule)


;;; Pure helpers ----------------------------------------------------------

(ert-deftest ledger-schedule/days-in-month-regular ()
  (should (= 31 (ledger-schedule-days-in-month 1 2024)))
  (should (= 28 (ledger-schedule-days-in-month 2 2023)))
  (should (= 30 (ledger-schedule-days-in-month 4 2024)))
  (should (= 31 (ledger-schedule-days-in-month 12 2024))))

(ert-deftest ledger-schedule/days-in-month-leap-year ()
  (should (= 29 (ledger-schedule-days-in-month 2 2024)))
  (should (= 29 (ledger-schedule-days-in-month 2 2000)))
  (should (= 28 (ledger-schedule-days-in-month 2 1900)))
  (should (= 28 (ledger-schedule-days-in-month 2 nil))))

(ert-deftest ledger-schedule/days-in-month-out-of-range ()
  (should-error (ledger-schedule-days-in-month 0 2024))
  (should-error (ledger-schedule-days-in-month 13 2024))
  (should-error (ledger-schedule-days-in-month -1 2024)))

(ert-deftest ledger-schedule/encode-day-of-week ()
  (should (= 0 (ledger-schedule-encode-day-of-week "Su")))
  (should (= 1 (ledger-schedule-encode-day-of-week "Mo")))
  (should (= 5 (ledger-schedule-encode-day-of-week "Fr")))
  (should (= 6 (ledger-schedule-encode-day-of-week "Sa")))
  (should-not (ledger-schedule-encode-day-of-week "XX")))


;;; Constraint forms ------------------------------------------------------

(defun ledger-schedule-test--funcall (form date)
  "Evaluate FORM (an expression that references the free variable `date')
with `date' bound to DATE.  Returns the form's value."
  (eval `(let ((date ',date)) ,form) t))

(ert-deftest ledger-schedule/constrain-day-zero-count ()
  ;; Match every Saturday: 2024-04-27 was a Saturday.
  (let ((form (ledger-schedule-constrain-day-in-month 0 6)))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 27 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 28 4 2024)))))

(ert-deftest ledger-schedule/constrain-day-positive-count ()
  ;; Third Thursday of month.  In April 2024 the 3rd Thursday is the 18th.
  (let ((form (ledger-schedule-constrain-day-in-month 3 4)))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 18 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 11 4 2024)))))

(ert-deftest ledger-schedule/constrain-day-negative-count ()
  ;; Last Friday of April 2024 is the 26th.
  (let ((form (ledger-schedule-constrain-day-in-month -1 5)))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 26 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 19 4 2024)))))

(ert-deftest ledger-schedule/constrain-day-out-of-range ()
  (should-error (ledger-schedule-constrain-day-in-month 7 0))
  (should-error (ledger-schedule-constrain-day-in-month 0 7))
  (should-error (ledger-schedule-constrain-day-in-month 0 -1)))

(ert-deftest ledger-schedule/constrain-every-count-day ()
  ;; Every second Friday starting 2024-04-12 (a Friday).
  (let* ((start (encode-time 0 0 0 12 4 2024))
         (form (ledger-schedule-constrain-every-count-day 5 2 start)))
    (should (ledger-schedule-test--funcall form start))
    ;; Two weeks later (2024-04-26) — also matches.
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 26 4 2024)))
    ;; One week later (2024-04-19) — should not match.
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 19 4 2024)))))

(ert-deftest ledger-schedule/constrain-every-count-day-mismatch ()
  ;; START-DATE is a Friday but day-of-week given is Monday — error.
  (let ((start (encode-time 0 0 0 12 4 2024)))
    (should-error (ledger-schedule-constrain-every-count-day 1 2 start))))

(ert-deftest ledger-schedule/constrain-date-range ()
  ;; The function applies STRICT inequalities to month and day independently,
  ;; so a date matches only when month1 < target-month < month2 AND
  ;; day1 < target-day < day2.  This test documents the current behaviour.
  (let ((form (ledger-schedule-constrain-date-range 3 5 5 25)))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 15 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 1 4 2024)))   ; day not > 5
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 15 3 2024))) ; month not > 3
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 15 5 2024))))) ; month not < 5


;;; Year / month / day descriptors ---------------------------------------

(ert-deftest ledger-schedule/constrain-year ()
  (should (eq t (ledger-schedule-constrain-year "*" "*" "*")))
  ;; Letter in day-desc returns t to indicate "year doesn't matter".
  (should (eq t (ledger-schedule-constrain-year "2024" "*" "1Mo")))
  ;; Numeric list of years.
  (let ((form (ledger-schedule-constrain-year "2024,2025" "*" "*")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 1 1 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 1 1 2026))))
  (should-error (ledger-schedule-constrain-year "" "*" "*")))

(ert-deftest ledger-schedule/constrain-month ()
  (should (eq t (ledger-schedule-constrain-month "*" "*" "*")))
  (should (eq t (ledger-schedule-constrain-month "*" "1" "1Mo")))
  ;; Even months.
  (let ((form (ledger-schedule-constrain-month "*" "E" "*")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 1 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 1 3 2024))))
  ;; Odd months.
  (let ((form (ledger-schedule-constrain-month "*" "O" "*")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 1 3 2024))))
  ;; Comma-separated.
  (let ((form (ledger-schedule-constrain-month "*" "1,7" "*")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 1 7 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 1 6 2024))))
  (should-error (ledger-schedule-constrain-month "*" "" "*")))

(ert-deftest ledger-schedule/constrain-day ()
  (should (eq t (ledger-schedule-constrain-day "*" "*" "*")))
  ;; Last day of month.
  (let ((form (ledger-schedule-constrain-day "*" "*" "L")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 30 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 29 4 2024))))
  ;; Numeric list.
  (let ((form (ledger-schedule-constrain-day "*" "*" "1,15")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 1 4 2024)))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 15 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 16 4 2024))))
  (should-error (ledger-schedule-constrain-day "*" "*" "")))

(ert-deftest ledger-schedule/parse-complex-date-nth-weekday ()
  ;; "1Mo" — first Monday — when given for April 2024 (1st Monday is the 1st).
  (let ((form (ledger-schedule-parse-complex-date "*" "*" "1Mo")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 1 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 8 4 2024)))))

(ert-deftest ledger-schedule/parse-complex-date-every-nth-weekday ()
  ;; "12+2Fr" — start on the 12th, every 2nd Friday from there.
  (let ((form (ledger-schedule-parse-complex-date "2024" "4" "12+2Fr")))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 12 4 2024)))
    (should (ledger-schedule-test--funcall
             form (encode-time 0 0 0 26 4 2024)))
    (should-not (ledger-schedule-test--funcall
                 form (encode-time 0 0 0 19 4 2024)))))


;;; Compilation pipeline --------------------------------------------------

(ert-deftest ledger-schedule/compile-constraints ()
  (let ((form (ledger-schedule-compile-constraints "*/*/*")))
    (should (eq 'and (car form)))
    ;; All three sub-forms are `t' (always match).
    (should (eq t (nth 1 form)))
    (should (eq t (nth 2 form)))
    (should (eq t (nth 3 form)))))

(ert-deftest ledger-schedule/transform-auto-tree-flat ()
  (let ((form (ledger-schedule-transform-auto-tree '("*/*/15"))))
    (should (eq 'lambda (car form)))
    ;; Funcalling on the 15th of any month should be true.
    (should (funcall form (encode-time 0 0 0 15 4 2024)))
    (should-not (funcall form (encode-time 0 0 0 14 4 2024)))))

(ert-deftest ledger-schedule/transform-auto-tree-nested ()
  ;; Nested tree case: recursion produces sublambdas which are dropped into an
  ;; `or' as literal objects; the result is therefore truthy for any date.
  ;; This documents the (questionable) current behaviour of the function.
  (let ((form (ledger-schedule-transform-auto-tree '(("*/*/15") ("*/*/L")))))
    (should (eq 'lambda (car form)))
    (should (funcall form (encode-time 0 0 0 15 4 2024)))
    (should (funcall form (encode-time 0 0 0 5 4 2024)))))

(ert-deftest ledger-schedule/read-descriptor-tree ()
  (let ((form (ledger-schedule-read-descriptor-tree "[*/*/1]")))
    (should (eq 'lambda (car form)))
    (should (funcall form (encode-time 0 0 0 1 4 2024)))
    (should-not (funcall form (encode-time 0 0 0 2 4 2024)))))


;;; End-to-end scan + buffer creation ------------------------------------

(defconst schedule-test--demo-schedule
  "[*/*/15] Bi-monthly Rent
    Expenses:Rent             $500
    Assets:Checking

[*/*/L] Internet Bill
    Expenses:Internet         $50
    Assets:Checking
"
  "Demo scheduled-transactions file.")

(defmacro schedule-test--with-schedule-file (&rest body)
  "Execute BODY with `schedule-file' bound to a temp ledger schedule file."
  (declare (indent 0) (debug t))
  `(let ((schedule-file (make-temp-file "ledger-schedule-test-" nil ".ledger")))
     (unwind-protect
         (progn
           (with-temp-file schedule-file
             (insert schedule-test--demo-schedule))
           ,@body)
       (delete-file schedule-file))))

(ert-deftest ledger-schedule/scan-transactions ()
  (schedule-test--with-schedule-file
   (let ((xacts (ledger-schedule-scan-transactions schedule-file)))
     (should (= 2 (length xacts)))
     ;; Each entry is (predicate-form payload-string).
     (dolist (x xacts)
       (should (= 2 (length x)))
       (should (eq 'lambda (car (car x))))
       (should (stringp (cadr x)))))))

(ert-deftest ledger-schedule/list-upcoming-xacts ()
  ;; Use a synthetic candidate list whose predicate matches every day.
  (let* ((always-true (eval '(lambda (_date) t) t))
         (candidates (list (list always-true "Daily Coffee"))))
    (let ((items (ledger-schedule-list-upcoming-xacts candidates 3 4)))
      ;; 3 days back + 4 days forward = 8 dates inclusive.
      (should (= 8 (length items)))
      ;; Each item is (date payload).
      (dolist (i items)
        (should (= 2 (length i)))
        (should (stringp (cadr i)))))))

(ert-deftest ledger-schedule/create-auto-buffer ()
  (let* ((always-true (eval '(lambda (_date) t) t))
         (candidates (list (list always-true "    Foo"))))
    (unwind-protect
        (let ((n (ledger-schedule-create-auto-buffer candidates 0 1)))
          (should (= 2 n)) ; 0 backward + 1 forward = 2 days
          (with-current-buffer ledger-schedule-buffer-name
            (should (string-match-p "Foo"
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))
            (should (eq major-mode 'ledger-mode))))
      (when (get-buffer ledger-schedule-buffer-name)
        (kill-buffer ledger-schedule-buffer-name)))))

(ert-deftest ledger-schedule/upcoming-no-file ()
  ;; Calling upcoming with a non-existent file should error.
  (should-error (ledger-schedule-upcoming "/no/such/file.ledger" 0 0)))

(ert-deftest ledger-schedule/upcoming-end-to-end ()
  (schedule-test--with-schedule-file
   (unwind-protect
       (progn
         ;; Emit into the schedule buffer.
         (ledger-schedule-upcoming schedule-file 30 30)
         (should (get-buffer ledger-schedule-buffer-name)))
     (when (get-buffer ledger-schedule-buffer-name)
       (kill-buffer ledger-schedule-buffer-name)))))


(ert-deftest ledger-schedule/upcoming-with-prefix-arg-prompts ()
  "With `current-prefix-arg' set, the interactive form prompts for parameters."
  (schedule-test--with-schedule-file
   (let ((current-prefix-arg t)
         (read-numbers (list 5 10)))
     (cl-letf (((symbol-function 'read-file-name)
                (lambda (&rest _) schedule-file))
               ((symbol-function 'read-number)
                (lambda (&rest _) (pop read-numbers))))
       (let ((interactive-form (interactive-form 'ledger-schedule-upcoming)))
         (should interactive-form)
         ;; Resolve the args via the interactive form, exercising the
         ;; prefix-arg branch.
         (let ((args (eval (cadr interactive-form) t)))
           (should (= 3 (length args)))
           (should (string= (car args) schedule-file))
           (should (= 5 (cadr args)))
           (should (= 10 (caddr args)))))))))


(ert-deftest ledger-schedule/parse-complex-date-letter-day ()
  "`ledger-schedule-constrain-day' returns a complex form for letter day-desc."
  ;; The function returns whatever `ledger-schedule-parse-complex-date'
  ;; returns when day-desc has a letter.  Exercises line 251.
  (let ((form (ledger-schedule-constrain-day "*" "*" "1Fr")))
    (should form)
    ;; Form should be evaluable.
    (should (or (eq (car form) 'and)
                (eq (car form) 'eq)
                (eq (car form) 'let)))))


(provide 'schedule-test)

;;; schedule-test.el ends here
