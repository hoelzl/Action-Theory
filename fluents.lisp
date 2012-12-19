;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))


;;; Fluent Definitions
;;; ==================

;;; The definition of fluents is provided by (indirect) instances of
;;; FLUENT-DEFINITION.

(defgeneric fluents (context)
  (:documentation
   "A hash table containing the description of every fluent in CONTEXT."))

;;; TODO: see (setf known-operators)
(defgeneric (setf fluents) (new-value context))

(defgeneric fluent-definition (fluent-name context &optional default)
  (:documentation
   "Returns the definition of the fluent FLUENT-NAME in CONTEXT.")
  (:method ((fluent-name symbol) (context compilation-context)
            &optional (default nil))
    (gethash fluent-name (fluents context) default)))

(defgeneric (setf fluent-definition) (new-value fluent-name context)
  (:documentation
   "Set the definition for fluent FLUENT-NAME in CONTEXT to NEW-VALUE.")
  (:method (new-value (fluent-name symbol) context)
    (setf (gethash fluent-name (fluents context)) new-value)))

(defclass fluent-definition (operator-mixin context-mixin)
  ((fluent-class
    :accessor fluent-class :initarg :class
    :initform (required-argument :class)
    :documentation "The class of this fluent.")
   (fluent-successor-state
    :accessor fluent-successor-state :initarg :successor-state
    :initform nil
    :documentation "The successor state axiom for this fluent."))
  (:documentation "The definition of a fluent."))


(defmethod initialize-instance :after
    ((self fluent-definition) &key context operator successor-state)
  (assert context (context)
          "Cannot create a fluent definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a fluent definition without operator.")
  (setf (fluent-definition operator context) self)
  (when (and successor-state (consp successor-state))
    (setf (slot-value self 'fluent-successor-state)
          (parse-into-term-representation successor-state context))))


(defclass relational-fluent-definition (fluent-definition)
  ()
  (:documentation "The definition of a relational fluent."))

(defgeneric declare-relational-fluent (operator context &optional class-name)
  (:documentation
   "Create a new instance of RELATIONAL-FLUENT-DEFINITION and assign it as
fluent definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (setf (fluent-definition operator context)
          (make-instance 'relational-fluent-definition
                         :operator operator :class class-name :context context))))

(defun define-relational-fluent (operator signature
                                 &key (class-name (symbolicate operator '#:-term))
                                      successor-state
                                      force-redefinition)
  (when (or (not (find-class class-name nil)) force-redefinition)
    (ensure-class class-name
                  :direct-superclasses '(known-general-application-term))
    (ensure-method #'operator `(lambda (term)
                                 (declare (ignore term))
                                 ',operator)
                   :specializers (list (find-class class-name)))
    (ensure-method #'declare-primitive-action
                   `(lambda (operator context &optional (class-name ',class-name))
                      (setf (fluent-definition operator context)
                            (make-instance 'relational-fluent-definition
                              :operator ',operator
                              :signature ',signature
                              :class class-name
                              :successor-state ',successor-state
                              :context context)))
                   :specializers (list (intern-eql-specializer operator)
                                       (find-class 'compilation-context)))))


(defclass functional-fluent-definition (fluent-definition)
  ()
  (:documentation "The definition of a functional fluent."))

(defgeneric declare-functional-fluent (operator context &optional class-name)
  (:documentation
   "Create a new instance of FUNCTIONAL-FLUENT-DEFINITION and assign it as
fluent definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context compilation-context)
            &optional (class-name (symbolicate operator '#:-term)))
    (setf (fluent-definition operator context)
          (make-instance 'functional-fluent-definition
                         :operator operator :class class-name :context context))))

(defun define-functional-fluent (operator signature
                                 &key (class-name (symbolicate operator '#:-term))
                                      successor-state
                                      force-redefinition)
  (when (or (not (find-class class-name nil)) force-redefinition)
    (ensure-class class-name
                  :direct-superclasses '(known-general-application-term))
    (ensure-method #'operator `(lambda (term)
                                 (declare (ignore term))
                                 ',operator)
                   :specializers (list (find-class class-name)))
    (ensure-method #'declare-primitive-action
                   `(lambda (operator context &optional (class-name ',class-name))
                      (setf (fluent-definition operator context)
                            (make-instance 'functional-fluent-definition
                              :operator ',operator
                              :signature ',signature
                              :class class-name
                              :successor-state ',successor-state
                              :context context)))
                   :specializers (list (intern-eql-specializer operator)
                                       (find-class 'compilation-context)))))

