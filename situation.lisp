;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defclass situation ()
  ()
  (:documentation
   "The superclass of all situations."))

(defclass initial-situation (situation)
  ()
  (:documentation
   "The initial situation, i.e., the root of a tree of situations."))

(defvar *the-initial-situation* (make-instance 'initial-situation))

(defun the-initial-situation ()
  *the-initial-situation*)

(defclass successor-situation (situation)
  ((action :accessor action :initarg :action
	   :initform (required-argument :action))
   (previous-situation :accessor previous-situation
		       :initarg :previous-situation
		       :initform (required-argument :previous-situation)
		       :type situation)))

(defgeneric in-situation (term situation)
  (:documentation
   "Replace the initial situation with SITUATION in TERM."))
