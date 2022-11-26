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
(defsystem "calcal"
  :description "calcal: Compute and convert dates from 11 calendars on the command line."
  :version "0.1.0"
  :author "Alexander Staudt"
  :licence "GPLv3+"
  :components ((:file "package")
               (:file "calendar" :depends-on ("package"))
               (:file "utility" :depends-on ("calendar"))
               (:file "cli" :depends-on ("utility")))
  :build-operation "program-op"
  :build-pathname "calcal"
  :entry-point "calcal::toplevel-main"
  :depends-on (#:asdf))
