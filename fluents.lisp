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
;;; FLUENT.

(defgeneric fluents (context)
  (:documentation
   "A hash table containing the description of every fluent in CONTEXT."))

;;; TODO: see (setf known-operators)
(defgeneric (setf fluents) (new-value context))

(defgeneric lookup-fluent (fluent-name context &optional default)
  (:documentation
   "Returns the definition of the fluent FLUENT-NAME in CONTEXT.")
  (:method ((fluent-name symbol) (context abstract-context)
            &optional (default nil))
    (gethash fluent-name (fluents context) default)))

(defgeneric (setf lookup-fluent) (new-value fluent-name context)
  (:documentation
   "Set the definition for fluent FLUENT-NAME in CONTEXT to NEW-VALUE.")
  (:method (new-value (fluent-name symbol) context)
    (setf (gethash fluent-name (fluents context)) new-value)))

(defclass fluent (operator-mixin context-mixin)
  ((successor-state-axiom
    :accessor successor-state-axiom :initarg :successor-state-axiom
    :initform nil
    :documentation "The successor state axiom for this fluent."))
  (:documentation "The definition of a fluent."))


(defmethod initialize-instance :after
    ((self fluent) &key context operator successor-state-axiom)
  (assert context (context)
          "Cannot create a fluent definition without context.")
  (assert (and operator (symbolp operator)) (operator)
          "Cannot create a fluent definition without operator.")
  (setf (lookup-fluent operator context) self)
  (when (and successor-state-axiom (consp successor-state-axiom))
    (setf (slot-value self 'successor-state-axiom)
          (parse-into-term-representation successor-state-axiom context))))


(defclass relational-fluent (fluent)
  ()
  (:documentation "The definition of a relational fluent."))

(defgeneric declare-relational-fluent (operator context &key class-name)
  (:documentation
   "Create a new instance of RELATIONAL-FLUENT and assign it as
fluent definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context abstract-context)
            &key (class-name (symbolicate operator '#:-term)))
    (setf (lookup-fluent operator context)
          (make-instance 'relational-fluent
                         :operator operator :class class-name :context context))))

(defun define-relational-fluent (operator signature
                                 &key (class-name (symbolicate operator '#:-term))
                                      successor-state-axiom
                                      force-redefinition)
  (when (or (not (find-class class-name nil)) force-redefinition)
    (ensure-class class-name
                  :direct-superclasses '(known-general-application-term))
    (ensure-method #'operator `(lambda (term)
                                 (declare (ignore term))
                                 ',operator)
                   :specializers (list (find-class class-name)))
    (ensure-method #'declare-relational-fluent
                   `(lambda (operator context &key (class-name ',class-name))
                      (setf (lookup-fluent operator context)
                            (make-instance 'relational-fluent
                              :operator ',operator
                              :signature ',signature
                              :class class-name
                              :successor-state-axiom ',successor-state-axiom
                              :context context)))
                   :specializers (list (intern-eql-specializer operator)
                                       (find-class 'abstract-context)))))


(defclass functional-fluent (fluent)
  ()
  (:documentation "The definition of a functional fluent."))

(defgeneric declare-functional-fluent (operator context &key class-name)
  (:documentation
   "Create a new instance of FUNCTIONAL-FLUENT and assign it as
fluent definition for OPERATOR in CONTEXT.")
  (:method ((operator symbol) (context abstract-context)
            &key (class-name (symbolicate operator '#:-term)))
    (setf (lookup-fluent operator context)
          (make-instance 'functional-fluent
                         :operator operator :class class-name :context context))))

(defun define-functional-fluent (operator signature
                                 &key (class-name (symbolicate operator '#:-term))
                                      successor-state-axiom
                                      force-redefinition)
  (when (or (not (find-class class-name nil)) force-redefinition)
    (ensure-class class-name
                  :direct-superclasses '(known-general-application-term))
    (ensure-method #'operator `(lambda (term)
                                 (declare (ignore term))
                                 ',operator)
                   :specializers (list (find-class class-name)))
    (ensure-method #'declare-functional-fluent
                   `(lambda (operator context &key (class-name ',class-name))
                      (setf (lookup-fluent operator context)
                            (make-instance 'functional-fluent
                              :operator ',operator
                              :signature ',signature
                              :class class-name
                              :successor-state-axiom ',successor-state-axiom
                              :context context)))
                   :specializers (list (intern-eql-specializer operator)
                                       (find-class 'abstract-context)))))

