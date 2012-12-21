;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defclass sdp-case ()
  ((case-term :accessor case-term :initarg :case-term)
   (value :accessor value :initarg :value)
   (alias :accessor alias :initarg :alias :initform nil)))

(defmethod print-object ((self sdp-case) stream)
  (with-slots (case-term value alias) self
    (format stream "(:term ~:W :value ~A~:[~; :alias ~:*~:W~])"
	    (to-sexpr case-term) value alias)))
	  

(defclass cases-term (known-application-term name-mixin)
  ((sdp-cases :accessor sdp-cases :initarg :sdp-cases
	      :initform '()))
  (:documentation
   "Representation of a case statement."))

(defmethod operator ((term cases-term))
  'cases)

(defmethod print-object ((self cases-term) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (sdp-cases self))))
