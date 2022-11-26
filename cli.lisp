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
  (format nil "~d~a~%" "Usage of calcal:
  -c string
        comma-separated list of calendars. Currently, calcal supports
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
  -d string
        date (format: yyyy-mm-dd). When omitted, '-d' defaults to the
        current date " 
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
         (info (nth 2 (assoc (date-calendar computed-date) ui-calendar-names :test #'string-equal)))
         (calendar (first (uiop:split-string info :max 2))))
    (format nil "~24,a~a~%" calendar date)))


;;; collect command line options
(defun get-h (argv)
  (if (member "-h" argv :test #'equal)
      (subseq (subseq argv (position "-h" argv :test #'equal)) 0 1)
      nil))


(defun get-c (argv)
  (if (member "-c" argv :test #'equal) ; check if argv contains -c flag
      ;; get subsequence following (and including) -c 
      (let ((ssq (subseq argv (position "-c" argv :test #'equal))))
        (append (list (nth 0 ssq))
                (if (and (> (length ssq) 1)
                         (not (string= (subseq (nth 1 ssq) 0 1) "-")))
                    (list (nth 1 ssq))
                    nil)))))


(defun get-d (argv)
  (if (member "-d" argv :test #'equal) ; check if argv contains -d flag
      ;; get subsequence following (and including) -d 
      (let ((ssq (subseq argv (position "-d" argv :test #'equal))))
        (append (list (nth 0 ssq))
                (if (and (> (length ssq) 1)
                         (numberp (parse-integer (nth 1 ssq)
                                                 :start 0
                                                 :end 2
                                                 :junk-allowed t)))
                    (list (nth 1 ssq))
                    nil)))))


(defun get-remaining-options (argv options)
  (remove-if #'(lambda (e) (member e options :test #'equal)) argv))


(defun parse-argv (argv)
  (let ((help (get-h argv))
        (date (get-d argv))
        (cals (get-c argv)))
    (pairlis '("help" "date" "calendars" "undefined")
             (list help date cals
                   (get-remaining-options argv (append help date cals))))))


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
         (help? (get-value "help" parsed-argv))
         (date? (get-value "date" parsed-argv))
         (date-arg (nth 1 (get-value "date" parsed-argv)))
         (date (parse-date-string date-arg))
         (cals? (get-value "calendars" parsed-argv))
         (cals-arg (nth 1 (get-value "calendars" parsed-argv)))
         (cals (if cals-arg
                   (parse-comma-separated-calendars cals-arg)
                   calendars))
         (undef-args (get-value "undefined" parsed-argv)))

    (cond
      (help? ;; -h flag
       (princ ui-display-help))

      (undef-args ;; undefined flags;
       ;; remaining (undefined) command line arguments are ignored
       (let ((undefined-flags
               (remove-if #'(lambda (e) (not (string= (subseq e 0 1) "-")))
                          undef-args)))
         (if (> (length undefined-flags) 1)
             (format t "Flags provided but not defined: ~{~a~^, ~}~%"
                     undefined-flags)
             (format t "Flag provided but not defined: ~d~%"
                     undefined-flags)))
       (princ ui-display-help))

      ;; -d flag, date missing
      ((and date? (not date-arg))
       (format t "Flag needs an argument: ~d~%" "-d")
       (princ ui-display-help))

      ;; -c flag, calendars missing
      ((and cals? (not cals-arg))
       (format t "Flag needs an argument: ~d~%" "-c")
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
  (main (uiop:command-line-arguments)))
