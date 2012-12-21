;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))


;;; Fluent Definitions
;;; ==================

;;; The definition of fluents is provided by (indirect) instances of
;;; FLUENT.

(defmethod lookup-table-accessor-for-type ((type (eql 'fluent)))
  'fluents)

(defgeneric successor-state-axiom (term)
  (:documentation
   "Returns the successor state axiom of TERM, or NIL if none exists."))

(defclass fluent (context-mixin prototype-mixin)
  ((successor-state-axiom
    :accessor successor-state-axiom :initarg :successor-state-axiom
    :initform nil
    :documentation "The successor state axiom for this fluent.")
   (result-sort :accessor result-sort))
  (:documentation "The definition of a fluent."))

(defmethod initialize-instance :after
    ((self fluent) &key prototype
                        sort
                        successor-state-axiom
                        context)
  (assert context (context)
          "Cannot create a fluent definition without context.")
  (setf (lookup (operator self) 'fluent context) self)
  (setf (result-sort self)
        (make-instance 'logical-sort
          :name sort :context context))
  (when (and successor-state-axiom
             (not (termp successor-state-axiom)))
    (let* ((new-context (nested-context-with-prototype-variables
                         context prototype))
           (successor-state-term (parse-into-term-representation
                                  successor-state-axiom new-context)))
      (setf (slot-value self 'successor-state-axiom)
            successor-state-term))))

(defun declare-fluent (&key prototype successor-state-axiom
                            (sort 'boolean)
                            (context *default-context*))
  "Create a new FLUENT and register it in context."
  (make-instance 'fluent
    :prototype prototype
    :sort sort
    :successor-state-axiom successor-state-axiom
    :context context))

(defmethod print-object ((self fluent) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (operator self))))

;;; TODO: Implement the following methods.  This requires us to have a more
;;; complete implementation of sorts first.

(defgeneric is-relational-fluent-p (thing)
  (:documentation
   "Returns true if THING is a relational fluent, false otherwise.")
  (:method (thing)
    (declare (ignore thing))
    nil))

(defgeneric is-functional-fluent-p (thing)
  (:documentation
   "Returns true if THING is a functional fluent, false otherwise.")
  (:method (thing)
    (declare (ignore thing))
    nil))
