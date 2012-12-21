;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defmethod lookup-table-accessor-for-type ((type (eql 'logical-sort)))
  'sorts)

(defclass logical-sort (name-mixin context-mixin)
  ()
  (:documentation
   "Representation of a logical sort."))

(defmethod initialize-instance :after ((self logical-sort) &key name context)
  (assert name (name)
	  "Cannot create an unnamed sort.")
  (assert context (context)
	  "Cannot create a sort without context.")
  (setf (lookup name 'logical-sort context) self))

(defun declare-sort (&key name (context *default-context*))
  (make-instance 'logical-sort
		 :name name
		 :context context))

(defmethod print-object ((self logical-sort) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (name self))))
