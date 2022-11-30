;;;; -*- mode: Lisp; indent-tabs-mode: nil; -*-
;;;; Copyright (C) 2022  Alexander Staudt
;;;; 
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;;
;;;; This file defines the command line interface calling functions defined in
;;;; calendar.lisp
(in-package :calcal)


;;; Data and helper functions
(defun convert-lisp-to-camel-case (string)
  ;; Convert a lisp-case-string into camelCase.
  (let ((name (uiop:split-string (string-downcase string) :max nil :separator "-")))
    (if (> (length name) 1)
        (format nil "~{~a~^~}"
                (concatenate
                 'list
                 (subseq name 0 1)
                 (mapcar #'string-capitalize (subseq name 1))))
        (nth 0 name))))


(defun find-upper-case-0 (string start)
  (let ((p (position-if #'upper-case-p string :start start)))
    (if (not p)
        (list p)
        (remove-if #'not
                   (append (list p)
                           (find-upper-case-0 string (+ p 1)))))))


(defun split-at-position-0 (string positions)
  (let ((start (car positions))
        (rest (cdr positions)))
    (if (not start)
        nil
        (remove-if #'(lambda (x) (equal x ""))
                   (append (list (subseq string start (car rest)))
                           (split-at-position-0 string rest))))))


(defun find-upper-case (string)
  (find-upper-case-0 string 0))


(defun split-at-position (string positions)
  (let ((pos (append (list 0) positions)))
    (split-at-position-0 string pos)))


(defun convert-camel-case-to-lisp (string)
  ;; Convert a camelCase string into lisp-case.
  (let ((s (split-at-position string (find-upper-case string))))
    (format nil "~{~a~^-~}" (mapcar #'string-downcase s))))


(defparameter ui-calendar-names
  ;; Pairlist with calendar name symbols, names, and descriptions
  (let ((calendar-descriptions
          (list "Gregorian calendar"
                "ISO calendar"
                "Julian calendar"
                "Islamic calendar"
                "Hebrew calendar"
                "Mayan Long Count calendar"
                "Mayan Haab calendar"
                "Mayan Tzolkin calendar"
                "French Revolutionary calendar"
                "Old Hindu Solar calendar"
                "Old Hindu Lunar calendar")))
    (reverse (pairlis
              calendars
              (mapcar #'list
                      (mapcar #'convert-lisp-to-camel-case calendars)
                      calendar-descriptions)))))


(defparameter ui-calendar-list
  ;; List of calendars specifiable with the -c flag.
  (concatenate 'list
               (pairlis '("all")
                        (list (list "all" "all calendars listed below (default)")))
               ui-calendar-names))


(defparameter ui-display-note
  ;; Cautonary note about limitations of the calendrical calculation functions.
  (format nil "~d~%" "Please note:
- for dates before 1 January 1 (Gregorian), calcal may return incorrect
  ISO and Julian dates
- for dates before the beginning of the Islamic calendar (16 July 622
  Julian), calcal returns an invalid Islamic date
- for dates before the beginning of the French Revolutionary calendar
  calendar (22 September 1792), calcal returns an invalid French date
- Old Hindu (solar and lunar) calendar dates may be off by one day"))


;;;
;;; Utility functions for date manipulation
;;;
(defun current-date ()
  ;; Get the current (system) date as list (list year month day)
  (let* ((time-list (multiple-value-list (get-decoded-time)))
         (year (nth 5 time-list))
         (month (nth 4 time-list))
         (day (nth 3 time-list)))
    (list year month day)))


;;; It would might be nicer if the help section for the -c flag could be
;;; generated automatically from a list of supported calendars
(defparameter ui-display-help
  (format nil "~d~a~%~%" "Usage: calcal [options]
Options:
  --calendar  comma-separated list of calendars.
              Currently, calcal supports:
                all             all calendars listed below (default)
                gregorian       Gregorian calendar
                iso             ISO calendar
                julian          Julian calendar
                islamic         Islamic calendar
                hebrew          Hebrew calendar
                mayanLongCount  Mayan Long Count calendar
                mayanHaab       Mayan Haab calendar
                mayanTzolkin    Mayan Tzolkin calendar
                french          French Revolutionary calendar
                oldHinduSolar   Old Hindu Solar calendar
                oldHinduLunar   Old Hindu Lunar calendar
  --date      date (format: yyyy-mm-dd). When omitted, '--date'
              defaults to the current date " 
          (let* ((date (current-date)) 
                 (year (nth 0 date)) 
                 (month (nth 1 date))
                 (day (nth 2 date))) 
            (format nil "(~d-~2,'0d-~2,'0d)" year month day))))


(defun parse-date-string (date-string)
  ;; Parse date strings. Expects ISO 8601 strings (yyyy-mm-dd). Returns nil if string can't be parsed.
  (if date-string ; make sure input is not nil
      (let* ((date-tokens (uiop:split-string date-string :max nil :separator "-"))
             (num-tokens (mapcar
                          #'(lambda (x) (parse-integer x :junk-allowed t))
                          date-tokens))
             (tokens
               (if (and (string= (subseq date-string 0 1) "-") ; deal with years BCE
                        (car (cdr num-tokens)))       
                   (append (list (* (1- 0) (car (cdr num-tokens))))
                           (cdr (cdr num-tokens)))
                   num-tokens)))
        ;; test if date list is valid:
        ;; all elements are not nil, months within (1, 12), and days within (1, 31)
        (cond ((member nil tokens) nil) 
              ((not (member (nth 1 tokens) '(1 2 3 4 5 6 7 8 9 10 11 12))) nil)
              ((or (< (nth 2 tokens) 1)
                   (> (nth 2 tokens) 31))
               nil)
              (t tokens)))
      nil))


(defun format-output (computed-date)
  (let* ((date (format-date computed-date))
         (info (nth 2 (assoc (date-calendar computed-date)
                             ui-calendar-names :test #'string-equal)))
         (calendar (first (uiop:split-string info :max 2))))
    (format nil "~24,a~a~%" calendar date)))


;;; collect command line options
(defun get-h-0 (arg)
  (let* ((argv (nth 1 arg))
         (resv (nth 0 arg))
         (help-flag (cond ((member "-h" argv :test #'equal) "-h")
                          ((member "--help" argv :test #'equal) "--help")
                          (t nil))))
    (cond ((not help-flag) nil)
          (t (list (append resv '(t))
                   (remove help-flag argv :test #'equal))))))


(defun get-h (arg)
  (let ((args (get-h-0 arg)))
    (if (not args)
        arg
        (get-h args))))


(defun get-c-0 (arg)
  (let* ((argv (nth 1 arg))
         (resv (nth 0 arg))
         (calendar-flag (cond ((member "-c" argv :test #'equal) "-c")
                              ((member "--calendar" argv :test #'equal) "--calendar")
                              (t nil))))
    (cond ((not calendar-flag) nil)
          (t (let ((subs-1 (subseq argv 0 (position calendar-flag argv :test #'equal)))
                   (subs-2 (subseq argv (position calendar-flag argv :test #'equal))))
               (if (and (> (length subs-2) 1)
                        (not (string= (subseq (nth 1 subs-2) 0 1) "-")))
                   (list (append resv (list (cadr subs-2)))
                         (append subs-1 (nthcdr 2 subs-2)))
                   (list (append resv '(nil))
                         (append subs-1 (nthcdr 1 subs-2)))))))))


(defun get-c (arg)
  (let ((args (get-c-0 arg)))
    (if (not args)
        arg
        (get-c args))))


(defun get-d-0 (arg)
  ;; check if argv contains -d/--date flag
  (let* ((argv (nth 1 arg))
         (resv (nth 0 arg))
         (date-flag (cond ((member "-d" argv :test #'equal) "-d")
                          ((member "--date" argv :test #'equal) "--date")
                          (t nil))))
    (cond ((not date-flag) nil)
          (t (let ((subs-1 (subseq argv 0 (position date-flag argv :test #'equal)))
                   (subs-2 (subseq argv (position date-flag argv :test #'equal))))
               (if (and (> (length subs-2) 1)
                        ;; allow dates BCE (i.e. digit with leading "-") and
                        ;; strings that do not start with "-"
                        (or (if (> (length (nth 1 subs-2)) 1)
                                (numberp (parse-integer (nth 1 subs-2)
                                                    :start 0
                                                    :end 2
                                                    :junk-allowed t))
                                nil)
                            (not (string= (subseq (nth 1 subs-2) 0 1) "-"))))
                   (list (append resv (list (cadr subs-2)))
                         (append subs-1 (nthcdr 2 subs-2)))
                   (list (append resv '(nil))
                         (append subs-1 (nthcdr 1 subs-2)))))))))


(defun get-d (arg)
  (let ((args (get-d-0 arg)))
    (if (not args)
        arg
        (get-d args))))


(defun parse-argv (argv)
  (let* ((arg (list nil argv))
         (parsed-help (get-h arg))
         (parsed-date (get-d (list nil (nth 1 parsed-help))))
         (parsed-cals (get-c (list nil (nth 1 parsed-date))))
         (date-arg (nth 0 parsed-date))
         (cals-arg (nth 0 parsed-cals))
         (rest-arg (nth 1 parsed-cals)))
    (pairlis '("help?" "date?" "date-arg" "cals?" "cals-arg" "undefined")
             (list (if (nth 0 parsed-help) t nil)
                   (if date-arg t nil)
                   (if (not (member nil date-arg))
                       (first date-arg)
                       nil)
                   (if cals-arg t nil)
                   (if (not (member nil cals-arg))
                       (first cals-arg)
                       nil)
                   ;; In some Lisp implementations, the runtime already defines
                   ;; the options "-c" (e.g. Clisp) and "-d" (e.g. Clozure CL),
                   ;; removing them from the argument list returned by
                   ;; uiop:command-line-arguments. With the "--" dummy flag
                   ;; preceding any further options, one may tell the Lisp
                   ;; runtime to directly pass those options to
                   ;; uiop:command-line-arguments.
                   (remove "--" rest-arg :test #'equal)))))


(defun get-value (key alist)
  (cdr (assoc key alist :test #'equal)))


(defun parse-comma-separated-calendars (string)
  (if string
      (let ((calendar-list
              (mapcar #'convert-camel-case-to-lisp
                      (uiop:split-string string :max nil :separator ","))))
        (remove-if #'(lambda (e) (not (member e calendars :test #'equal)))
                   calendar-list))
      nil))


(defun main (argv)
  (let* ((parsed-argv (parse-argv argv))
         (help? (get-value "help?" parsed-argv))
         (date? (get-value "date?" parsed-argv))
         (date-arg (get-value "date-arg" parsed-argv))
         (date (parse-date-string date-arg))
         (cals? (get-value "cals?" parsed-argv))
         (cals-arg (get-value "cals-arg" parsed-argv))
         (cals (if cals-arg
                   (parse-comma-separated-calendars cals-arg)
                   calendars))
         (undef-args (get-value "undefined" parsed-argv))
         (undefined-flags (remove-if #'(lambda (e) (not (string= (subseq e 0 1) "-")))
                                     undef-args)))
    (cond
      (help? ;; -h flag
       (princ ui-display-help))

      (undefined-flags ;; undefined flags;
       ;; remaining (undefined) command line arguments are ignored
       (if (> (length undefined-flags) 1)
           (format t "Flags provided but not defined: ~{~a~^, ~}~%"
                   undefined-flags)
           (format t "Flag provided but not defined: ~d~%"
                   (nth 0 undefined-flags)))
       (princ ui-display-help))

      ;; -d flag, date missing
      ((and date? (not date-arg))
       (format t "Flag needs an argument: ~d~%" "--date")
       (princ ui-display-help))

      ;; -c flag, calendars missing
      ((and cals? (not cals-arg))
       (format t "Flag needs an argument: ~d~%" "--calendar")
       (princ ui-display-help))
      
      ;; -d flag, valid date
      ((and date? date)
       (princ ui-display-note)
       (terpri)
       (mapcar #'princ (mapcar #'format-output
                               (compute-dates date cals))))
      
      ;; -d flag, invalid date
      ((and date? date-arg (not date))
       (format t "Invalid date. Specify date as yyyy-mm-dd.~%"))

      ;; no -d flag
      ((not date?)
       (princ ui-display-note)
       (terpri)
       (mapcar #'princ (mapcar #'format-output
                               (compute-dates (current-date) cals)))))))


(defun toplevel-main ()
  (main (uiop:command-line-arguments))
  (uiop:quit))


