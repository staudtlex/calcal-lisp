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
;;;; This file provides utility functions for easier conversion from ISO 8601
;;;; dates to absolute (fixed) dates, and conversion from absolute dates to
;;;; various calendars
(in-package :calcal)

;;;
;;; Helper functions
;;;
(defun mdy-from-ymd (iso-8601-date)
  ;; Convert a YMD (ISO 8601) date to a MDY date
  (list (nth 1 iso-8601-date) (nth 2 iso-8601-date) (nth 0 iso-8601-date)))


(defun seq (from to)
  ;; Generate a sequence of consecutive integers
  (let ((length (1+ (- to from))))
    (loop :for n :below length :collect (+ from n))))


;;;
;;; Data and wrapper functions for easier date conversion
;;;
(defparameter calendars
  ;; List of calendar names
  '("gregorian"
    "iso"
    "julian"
    "islamic"
    "hebrew"
    "mayan-long-count"
    "mayan-haab"
    "mayan-tzolkin"
    "french"
    "old-hindu-solar"
    "old-hindu-lunar"))


(defstruct date
  ;; A structure with date components and calendar information.
  (calendar nil)
  (components nil))


(defun get-function-with-name (string)
  (do-symbols (s 'calcal)
    (if (and (search string (symbol-name s) :test #'string-equal) (fboundp s))
        (return s))))


(defun make-function-map (calendar-list)
  (let* ((function-name-strings
           (mapcar #'(lambda (c) (concatenate 'string c "-from-absolute"))
                   calendar-list))
         (function-names
           (mapcar #'get-function-with-name
                   function-name-strings)))
    (reverse (pairlis calendar-list function-names))))


(defparameter *function-map* (make-function-map calendars))


(defun from-absolute (absolute-date to-calendar)
  ;; Convert an absolute (fixed) date to a date representation specified by to-calendar.
  (let ((f (cdr (assoc to-calendar *function-map* :test #'string-equal))))
    (if (not (equal f nil))
        (make-date :calendar to-calendar
                   :components (funcall f absolute-date))
        (make-date))))


(defun absolute-from-ymd (iso-8601-date)
  ;; Get the absolute (fixed) date from an ISO 8601 date (of type (list year month day)).
  (absolute-from-gregorian (mdy-from-ymd iso-8601-date)))


(defun compute-dates (iso-8601-date calendar-list)
  ;; Compute all available calendar representations of a given absolute (fixed) date.
  (let ((absolute-date (absolute-from-ymd iso-8601-date)))
    (mapcar #'(lambda (cal) (from-absolute absolute-date cal)) calendar-list)))


;;;
;;; Data and functions for date formatting
;;;
(defparameter gregorian-months
  (reverse (pairlis
            (seq 1 12)
            (list
             "January" "February" "March"
             "April" "May" "June"
             "July" "August" "September"
             "October" "November" "December"))))

(defparameter islamic-months
  (reverse (pairlis
            (seq 1 12)
            (list
             "Muharram" "Safar" "Rabi I"
             "Rabi II" "Jumada I" "Jumada II"
             "Rajab" "Sha' Ban" "Ramadan"
             "Shawwal" "Dhu al-Qada" "Dhu al-Hijjah"))))

(defparameter hebrew-months
  (reverse (pairlis
            (seq 1 12)
            (list
             "Nisan" "Iyyar" "Sivan"
             "Tammuz" "Av" "Elul"
             "Tishri" "Heshvan" "Kislev"
             "Teveth" "Shevat" "Adar"))))

(defparameter haab-months
  (reverse (pairlis
            (seq 1 18)
            (list
             "Pop" "Uo" "Zip"
             "Zotz" "Tzec" "Xul"
             "Yaxkin" "Mol" "Chen"
             "Yax" "Zac" "Ceh"
             "Mac" "Kankin" "Muan"
             "Pax" "Kayab" "Cumku"))))

(defparameter tzolkin-names
  (reverse (pairlis
            (seq 1 20)
            (list
             "Imix" "Ik" "Akbal" "Kan"
             "Chiccan" "Cimi" "Manik" "Lamat"
             "Muluc" "Oc" "Chuen" "Eb"
             "Ben" "Ix" "Men" "Cib"
             "Caban" "Etznab" "Cauac" "Ahau"))))

(defparameter french-months
  (reverse (pairlis
            (seq 1 13)
            (list
             "Vendémiaire" "Brumaire" "Frimaire"
             "Nivôse" "Pluviôse" "Ventôse"
             "Germinal" "Floréal" "Prairial"
             "Messidor" "Thermidor" "Fructidor"
             "Sansculottides"))))

(defparameter old-hindu-solar-months
  (reverse (pairlis
            (seq 1 12)
            (list
             "Mesha" "Vrshabha" "Mithuna"
             "Karka" "Simha" "Kanya"
             "Tula" "Vrischika" "Dhanus"
             "Makara" "Kumbha" "Mina"))))

(defparameter old-hindu-lunar-months
  (reverse (pairlis
            (seq 1 12)
            (list
             "Chaitra" "Vaisakha" "Jyaishtha"
             "Ashadha" "Sravana" "Bhadrapada"
             "Asvina" "Kartika" "Margasira"
             "Pausha" "Magha" "Phalguna"))))


(defun format-gregorian (date)
  (let* ((components (date-components date))
         (day (nth 1 components))
         (month (nth 0 components))
         (month-name (cdr (assoc month gregorian-months)))
         (year (nth 2 components)))
    (format nil "~d ~a ~d" day month-name year)))


(defun format-julian (date)
  (format-gregorian date))


(defun format-iso (date)
  (let* ((components (date-components date))
         (day-of-week (nth 1 components))
         (week (nth 0 components))
         (year (nth 2 components)))
    (format nil "~d-W~2,'0d-~d" year week day-of-week)))


(defun format-islamic (date)
  (let* ((components (date-components date))
         (day (nth 1 components))
         (month (nth 0 components))
         (month-name (cdr (assoc month islamic-months)))
         (year (nth 2 components)))
    (format nil "~d ~a ~d" day month-name year)))


(defun format-hebrew (date)
  (let* ((components (date-components date))
         (day (nth 1 components))
         (month (nth 0 components))
         (year (nth 2 components))
         (month-name
           (cond
             ((and (equalp month 12) (hebrew-leap-year year)) "Adar I")
             ((and (equalp month 13) (hebrew-leap-year year)) "Adar II")
             (t (cdr (assoc month hebrew-months))))))
    (format nil "~d ~a ~d" day month-name year)))


(defun format-mayan-long-count (date)
  (let* ((components (date-components date))
         (baktun (nth 0 components))
         (katun (nth 1 components))
         (tun (nth 2 components))
         (uinal (nth 3 components))
         (kin (nth 4 components)))
    (format nil "~d.~d.~d.~d.~d" baktun katun tun uinal kin)))


(defun format-mayan-haab (date)
  (let* ((components (date-components date))
         (day (nth 0 components))
         (month (nth 1 components))
         (month-name (cdr (assoc month haab-months))))
    (format nil "~d ~a" day month-name)))


(defun format-mayan-tzolkin (date)
  (let* ((components (date-components date))
         (day (nth 0 components))
         (tzolkin-name (nth 1 components))
         (name (cdr (assoc tzolkin-name tzolkin-names))))
    (format nil "~d ~a" day name)))


(defun format-french (date)
  (let* ((components (date-components date))
         (day (nth 1 components))
         (month (nth 0 components))
         (month-name (cdr (assoc month french-months)))
         (year (nth 2 components)))
    (format nil "~d ~a an ~d" day month-name year)))


(defun format-old-hindu-solar (date)
  (let* ((components (date-components date))
         (day (nth 1 components))
         (month (nth 0 components))
         (month-name (cdr (assoc month old-hindu-solar-months)))
         (year (nth 2 components)))
    (format nil "~d ~a ~d" day month-name year)))


(defun format-old-hindu-lunar (date)
  (let* ((components (date-components date))
         (day (nth 2 components))
         (month (nth 0 components))
         (month-name (cdr (assoc month old-hindu-lunar-months)))
         (year (nth 3 components)))
    (format nil "~d ~a ~d" day month-name year)))


(defun format-date (date)
  (let ((f (get-function-with-name
            (concatenate 'string "format-" (date-calendar date)))))
    (funcall f date)))

