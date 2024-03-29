;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))


;;; Singelton Terms Mixin
;;; ======================

(defclass singleton-terms-mixin ()
  ((the-empty-program-term
    :initform nil :initarg :the-empty-program-term
    :documentation "Storage for the empty program term.")
   (the-no-operation-term
    :initform nil :initarg :the-no-operation-term
    :documentation "Storage for the no-operation term."))
  (:documentation
   "A mixin that provides storage and default implementation of getters for
   singleton terms of a context."))

(defmethod the-empty-program-term ((self singleton-terms-mixin))
  "Return the empty program term.  Cache the value in SELF."
  (or (slot-value self 'the-empty-program-term)
      (setf (slot-value self 'the-empty-program-term)
            (make-instance 'empty-program-term :context self))))

(defmethod the-no-operation-term ((self singleton-terms-mixin))
  "Return the no-operation term.  Cache the value in SELF."
  (or (slot-value self 'the-no-operation-term)
      (setf (slot-value self 'the-no-operation-term)
            (make-instance 'no-operation-term :context self))))


;;; Unique Terms Mixin
;;; ==================

(defclass terms-with-unique-names-mixin ()
  ((terms-with-unique-names
    :accessor terms-with-unique-names
    :initarg :terms-with-unique-names
    :initform (make-array '(16) :element-type 'term
                                :adjustable t :fill-pointer 0)))
  (:documentation
   "A mixin that provides storage for terms for which unique names axioms
   should be generated."))

(defmethod add-to-terms-with-unique-names
    ((term term-with-unique-name-mixin) (context terms-with-unique-names-mixin))
  (let ((position (position term (terms-with-unique-names context) 
                            :test (lambda (lhs rhs)
                                    (eql (name lhs) (name rhs))))))
    (if position
        (setf (aref (terms-with-unique-names context) position) term)
        (vector-push-extend term (terms-with-unique-names context)))))

;;; Compilation units
;;; =================

;;; A compilation unit is a long-lasting context.

(defvar *default-known-operators*
  (append *logical-operators*
          *programming-operators*
          *declaration-operators*
          *definition-operators*))

(defun default-known-operators ()
  (plist-hash-table *default-known-operators*))

(defvar *default-primitive-action-names*
  '(no-operation))

(defun default-primitive-action-names ()
  *default-primitive-action-names*)

(defclass compilation-unit
    (abstract-context singleton-terms-mixin terms-with-unique-names-mixin)
  ((declarations 
    :accessor declarations :initarg :declarations
    :initform (make-array '(10) :adjustable t :fill-pointer 0)
    :documentation "Logical declarations for this compilation unit.")
   (declared-operator-sorts
    :accessor declared-operator-sorts :initarg :declared-operator-sorts
    :initform (make-hash-table)
    :documentation "Hash table mapping operators to their declared sorts.")
   (known-operators
    :accessor known-operators :initarg :known-operators
    :initform (default-known-operators)
    :documentation "Special operators for this compilation unit.")
   (sorts
    :accessor sorts :initarg :sorts
    :initform (make-hash-table)
    :documentation "Hash table mapping sort names to definitions.")
   (primitive-actions
    :accessor primitive-actions :initarg :primitive-actions
    :initform (make-hash-table)
    :documentation
    "Hash table mapping the name of each primitive action to its corresponding
    PRIMITIVE-ACTION object.")
   (natures-choice-table
    :accessor natures-choice-table :initarg :natures-choice-table
    :initform (make-hash-table)
    :documentation
    "Hash table mapping the name of each nature's choice to its corresponding
    NATURES-CHOICE object.")
   (reward-function-table
    :accessor reward-function-table :initarg :reward-function-table
    :initform (make-hash-table)
    :documentation
    "Hash table mapping the name of each reward function to its corresponding
    REWARD-FUNCTION object.")
   (fluents
    :accessor fluents :initarg :fluents
    :initform (make-hash-table)
    :documentation
    "Hash table mapping the name of each fluent to its FLUENT object.")
   (variable-hash-table
    :accessor variable-hash-table :initarg :variable-hash-table
    :initform (make-hash-table)
    :documentation "Hash table for interning variables.")
   (number-hash-table
    :accessor number-hash-table :initarg :number-hash-table
    :initform (make-hash-table)
    :documentation "Hash table for interning numbers.")
   (functor-hash-table
    :accessor functor-hash-table :initarg :functor-hash-table
    :initform (make-hash-table)
    :documentation "Hash table for interning functors")
   (constants-for-sort-table
    :accessor constants-for-sort-table :initarg :constants-for-sort-table
    :initform (make-hash-table)
    :documentation "Hash table mapping sorts to their constants."))
  (:documentation
   "A single compilation unit."))


(defmethod print-object ((self compilation-unit) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "Vars: ~:W"
            (mapcar #'unique-name (variables self)))))

#+(or)
(defmethod shared-initialize :after ((self compilation-unit) slot-names &key)
  (declare (ignore slot-names))
    (dolist (action (default-primitive-action-names))
    (declare-primitive-action action self)))

(defmethod lookup-variable (name sort (context compilation-unit) &optional (create? t))
  (let ((hash-table (slot-value context 'variable-hash-table)))
    (multiple-value-bind (variable exists?)
	(gethash name hash-table nil)
      (cond (exists? variable)
	    (create?
	     (setf (lookup-variable name sort context)
		   (make-variable-term name sort context :intern nil)))
	    (t nil)))))

(defmethod (setf lookup-variable) (new-value name sort (context compilation-unit))
  (declare (ignore sort))
  (check-type new-value variable-term)
  (let ((hash-table (slot-value context 'variable-hash-table)))
    (setf (gethash name hash-table) new-value)))

(defmethod lookup-number (value (context compilation-unit) &optional (create? t))
  (let* ((hash-table (slot-value context 'number-hash-table)))
    (multiple-value-bind (number exists?)
	(gethash value hash-table nil)
      (cond (exists? number)
	    (create?
	     (setf (lookup-number value context)
		   (make-instance
		    'number-term :value value :intern nil :context context)))
	    (t nil)))))

(defmethod (setf lookup-number) (new-value value (context compilation-unit))
  (check-type new-value number-term)
  (assert (= (value new-value) value) (new-value)
          "Trying to set number ~A to value ~A." value (value new-value))
  (let* ((hash-table (slot-value context 'number-hash-table)))
    (setf (gethash value hash-table) new-value)))


(defmethod lookup-functor (name arity (context compilation-unit) &optional (create? t))
  (let* ((hash-table-1 (slot-value context 'functor-hash-table))
	 (hash-table-2 (gethash* name hash-table-1 (make-hash-table))))
    (multiple-value-bind (functor exists?)
	(gethash arity hash-table-2 nil)
      (cond (exists? functor)
	    (create?
	     (setf (lookup-functor name arity context)
		   (make-instance
		    'functor-term :name name :arity arity
				  :intern nil :context context)))
	    (t nil)))))

(defmethod (setf lookup-functor) (new-value name arity (context compilation-unit))
  (check-type new-value functor-term)
  (let* ((hash-table-1 (slot-value context 'functor-hash-table))
	 (hash-table-2 (gethash* name hash-table-1 (make-hash-table))))
    (setf (gethash arity hash-table-2) new-value)))


;;; Local Context
;;; =============

(defclass local-context (abstract-context)
  ((enclosing-context :accessor enclosing-context :initarg :enclosing-context
                  :initform (required-argument :enclosing-context))
   (local-variables :accessor local-variables :initarg :local-variables
                    :initform '()))
  (:documentation
   "A temporary context for forms that bind variables."))

(defmethod variables ((self local-context))
  (append (mapcar #'cdr (local-variables self))
          (variables (enclosing-context self))))

(defmethod print-object ((self local-context) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "Vars: ~:W"
            (mapcar #'unique-name (variables self)))))

(defmethod lookup-variable (name sort (context local-context) &optional (create? t))
  (let ((local-binding (assoc name (local-variables context))))
    (if local-binding
        (cdr local-binding)
        (let* ((enclosing-context (enclosing-context context))
               (outer-var (lookup-variable name sort enclosing-context nil)))
          (or outer-var
              (if create?
                  (let ((var (make-variable-term name sort context :intern nil)))
                    (push (cons name var) (local-variables context))
                    var)
                  nil))))))

(defmethod (setf lookup-variable) (new-value name sort (context local-context))
  (declare (ignore sort))
  (let ((binding (assoc name (local-variables context))))
    (cond (binding
           (setf (cdr binding) new-value))
          (t
           (push (cons name new-value) (local-variables context))
           new-value))))

(defmethod lookup-number (value (context local-context) &optional (create? t))
  (lookup-number value (enclosing-context context) create?))

(defmethod (setf lookup-number) (new-value value (context local-context))
  (assert (= (value new-value) value) (new-value)
          "Trying to set number ~A to value ~A." value (value new-value))
  (setf (lookup-number value (enclosing-context context)) new-value))

(defmethod lookup-functor (name arity (context local-context) &optional (create? t))
  (lookup-functor name arity (enclosing-context context) create?))

(defmethod (setf lookup-functor) (new-value name arity (context local-context))
  (setf (lookup-functor name arity (enclosing-context context)) new-value))

(define-delegates local-context enclosing-context
  (declarations (context))
  ((setf declarations) (new-declarations context))
  (declared-operator-sorts (context))
  ((setf declared-operator-sorts) (new-value context))
  (known-operators (context))
  ((setf known-operators) (new-value context))
  (primitive-actions (context))
  ((setf primitive-actions) (new-value context)
                            :new-value-type list)
  (natures-choice-table (context))
  (reward-function-table (context))
  (sorts (context))
  ((setf sorts) (new-value context))
  (fluents (context))
  ((setf fluents) (new-value context))
  (the-empty-program-term (context))
  (the-no-operation-term (context)))


;;; Top-Level Contexts
;;; ==================

              
(defclass top-level-context (compilation-unit)
  ()
  (:documentation
   "The state of a top-level interpreter."))

(unless (boundp '*default-context*)
  (setf *default-context* (make-instance 'top-level-context)))

(defgeneric top-level-context (context)
  (:documentation
   "Return the top-level context that encloses CONTEXT.")
  (:method ((context top-level-context))
    context)
  (:method ((context local-context))
    (top-level-context (enclosing-context context))))

;;; Some utilities for interactive exploration
;;; ==========================================

(defun default-known-classes ()
  (let ((result '()))
    (doplist (op class *default-known-operators*)
      (declare (ignorable op))
      (pushnew class result))
    result))

(defun default-known-cpls ()
  (mapcar (lambda (class)
            (cons class 
                  (ignore-errors
                   #-(or ecl abcl)
                   (c2mop:class-precedence-list (find-class class))
                   #+ecl
                   (clos:class-precedence-list (find-class class))
                   #+abcl
                   (mop:class-precedence-list (find-class class)))))
          (default-known-classes)))
