;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(declaim (ftype (function (t t) t) parse-into-term-representation))

;;; Primitive Action Definitions
;;; ============================

(defmethod lookup-table-accessor-for-type ((type (eql 'primitive-action)))
  'primitive-actions)

(defclass primitive-action (context-mixin prototype-mixin)
  ((precondition
    :reader precondition :initarg :precondition
    :initform 'true
    :documentation "The (right-hand side of the) precondition for this action.")
   (natures-choices
    :accessor natures-choices :initarg :natures-choices
    :initform '()
    :documentation "The nature's choices for this action."))
  (:documentation
   "The definition of a primitive action."))

(defmethod initialize-instance :after
    ((self primitive-action)
     &key context prototype precondition natures-choices)
  (assert context (context)
          "Cannot create a primitive action definition without context.")
  (setf (lookup (operator self) 'primitive-action context) self)
  (when (and precondition (not (termp precondition)))
    (let* ((new-context (nested-context-with-prototype-variables
                         context prototype))
           (precondition-term (parse-into-term-representation
                               precondition new-context)))
      (setf (slot-value self 'precondition) precondition-term)))
  (when natures-choices
    (setf (natures-choices self)
          (mapcar (lambda (name)
                    (lookup name 'natures-choice context))
                  natures-choices))))

(defun declare-primitive-action (&key prototype precondition natures-choices
                                      (context *default-context*))
  "Create a new instance of PRIMITIVE-ACTION and register it in
  CONTEXT."
  (make-instance 'primitive-action
    :prototype prototype
    :precondition precondition
    :natures-choices natures-choices
    :context context))
