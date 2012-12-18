;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-terms
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Primitive Action Definitions
;;; ============================

(defgeneric primitive-action-definition (action-name context &optional default)
  (:documentation 
   "Returns the definition of the primitive action ACTION-NAME in CONTEXT.")
  (:method ((action-name symbol) (context compilation-context)
            &optional (default nil))
    (gethash action-name (primitive-actions context) default)))

(defgeneric (setf primitive-action-definition) (new-value action-name context)
  (:documentation
   "Set the definition for primitive action ACTION-NAME in CONTEXT to
NEW-VALUE.")
  (:method (new-value (action-name symbol) context)
    (setf (gethash action-name (primitive-actions context)) new-value)))

(defclass primitive-action-definition (operator-mixin context-mixin)
  ((action-class
    :accessor action-class :initarg :class
    :initform (required-argument :class)
    :documentation "The class of this primitive action.")
   (action-precondition
    :reader action-precondition :initarg :precondition
    :initform nil
    :documentation "The precondition for this action.")
   (action-signature
    :accessor action-signature :initarg :signature
    :initform (required-argument :signature)
    :documentation "The signature of this action."))
  (:documentation
   "The definition of a primitive action."))

(defmethod initialize-instance :after
    ((self primitive-action-definition) &key context operator precondition)
  (assert context (context)
          "Cannot create a primitive action definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a primitive action definition without operator.")
  (setf (primitive-action-definition operator context) self)
  (when (and precondition (consp precondition))
    (let ((precondition-term (parse-into-term-representation
                              `(assert ',precondition) context)))
      (setf (slot-value self 'action-precondition) precondition-term))))

(defgeneric declare-primitive-action (operator context &optional class-name)
  (:documentation
   "Create a new instance of PRIMITIVE-ACTION-DEFINITION and assign it as
primitive-action definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (cerror "Continue anyway."
            "Declaring undefined primitive action.")
    (setf (primitive-action-definition operator context)
          (make-instance 'primitive-action-definition
            :operator operator :class class-name :context context))))

(defun define-primitive-action (operator signature
                                &key (class-name  (symbolicate operator '#:-term))
                                     precondition)
  (c2mop:ensure-class class-name :direct-superclasses '(primitive-action-term))
  (define-method 'operator
    :specializers (list (find-class class-name))
    :lambda-list '(term)
    :body `(lambda (term)
             (declare (ignore term))
             ',operator))
  (define-method 'declare-primitive-action
    :specializers (list (c2mop:intern-eql-specializer operator)
                        (find-class 'compilation-context))
    :lambda-list `(operator context &optional (class-name ',class-name))
    :body `(lambda (operator context &optional (class-name ',class-name))
             (setf (primitive-action-definition operator context)
                   (make-instance 'primitive-action-definition
                     :operator ',operator
                     :signature ',signature
                     :class class-name
                     :precondition ',precondition
                     :context context)))))

