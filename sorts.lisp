;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))


(define-condition no-declaration-for-sort
    (action-theory-error)
  ((name :initarg :name)
   (context :initarg :context))
  (:report (lambda (condition stream)
             (with-slots (name context) condition 
               (format stream "No sort ~A in context ~:W"
                       name context)))))

(defgeneric lookup-sort (sort-name context &optional default)
  (:documentation
   "Returns the definition of sort SORT-NAME in CONTEXT.  Signals an
   error if no sort with SORT-NAME exists and no DEFAULT is
   supplied.")
  (:method ((sort-name symbol) (context abstract-context)
	    &optional (default nil default-supplied-p))
    (or (gethash sort-name (sorts context) nil)
	(if default-supplied-p
	    default
	    (cerror "Return NIL."
		    'no-declaration-for-sort
		    :name sort-name :context context)))))

(defgeneric (setf lookup-sort) (new-value sort-name context)
  (:documentation
   "Set the definition of sort SORT-NAME in CONTEXT to NEW-VALUE.")
  (:method (new-value (sort-name symbol) (context abstract-context))
    (setf (gethash sort-name (sorts context)) new-value)))
    

(defclass logical-sort (name-mixin context-mixin)
  ()
  (:documentation
   "Representation of a logical sort."))

(defmethod initialize-instance :after ((self logical-sort) &key name context)
  (assert name (name)
	  "Cannot create an unnamed sort.")
  (assert context (context)
	  "Cannot create a sort without context.")
  (setf (lookup-sort name context) self))

(defun declare-sort (&key name (context *default-context*))
  (make-instance 'logical-sort
		 :name name
		 :context context))

(defmethod print-object ((self logical-sort) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (name self))))
