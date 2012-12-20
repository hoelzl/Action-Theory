;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Primitive Action Definitions
;;; ============================


(define-condition no-definition-for-primitive-action
    (action-theory-error)
  ((name :initarg :name)
   (context :initarg :context))
  (:report (lambda (condition stream)
             (with-slots (name context) condition 
               (format stream "No primitive action ~A in context ~:W"
                       name context)))))

(defgeneric lookup-primitive-action (action-name context &optional default)
  (:documentation 
   "Returns the definition of the primitive action ACTION-NAME in
   CONTEXT.  Signals an error if no primitive action exists and no
   DEFAULT is supplied.")
  (:method ((action-name symbol) (context abstract-context)
            &optional (default nil default-supplied-p))
    (or (gethash action-name (primitive-actions context) nil)
        (if default-supplied-p
            default
            (cerror "Return NIL."
                    'no-definition-for-primitive-action
                    :name action-name :context context)))))

(defgeneric (setf lookup-primitive-action) (new-value action-name context)
  (:documentation
   "Set the definition for primitive action ACTION-NAME in CONTEXT to
   NEW-VALUE.")
  (:method (new-value (action-name symbol) (context abstract-context))
    (setf (gethash action-name (primitive-actions context)) new-value)))

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

(defmethod operator ((action primitive-action))
  (first (prototype action)))

(defmethod initialize-instance :after
    ((self primitive-action) &key context prototype precondition)
  (assert context (context)
          "Cannot create a primitive action definition without context.")
  (setf (lookup-primitive-action (operator self) context) self)
  (when precondition
    (let* ((new-context (nested-context-with-prototype-variables
                         context prototype))
           (precondition-term (parse-into-term-representation
                               precondition new-context)))
      (setf (slot-value self 'precondition) precondition-term))))

(defmethod lookup-primitive-action
    ((definition primitive-action) context &optional default)
  (declare (ignore context default))
  definition)


(define-condition declaring-undefined-primitive-action (runtime-error)
  ((operator :initarg :operator))
  (:report (lambda (condition stream)
             (with-slots (operator) condition
               (format stream "Declaring undefined primitive action ~A."
                       operator)))))

#+(or)
(defgeneric declare-primitive-action (operator context &optional class-name)
  (:documentation
   "Create a new instance of PRIMITIVE-ACTION and assign it as
primitive-action definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context abstract-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (cerror "Create a direct instance of PRIMITIVE-ACTION."
            'declaring-undefined-primitive-action
            :operator operator)
    (setf (lookup-primitive-action operator context)
          (make-instance 'primitive-action
            :operator operator :class class-name :context context))))

(defun declare-primitive-action (&key prototype precondition natures-choices
                                      (context *default-context*))
  "Create a new instance of PRIMITIVE-ACTION and register it as
  primitive-action definition in CONTEXT."
  (make-instance 'primitive-action
    :prototype prototype
    :precondition precondition
    :natures-choices natures-choices
    :context context))
