;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Compilation Context
;;; ===================

;;; A compilation context encapsulates the information needed to parse
;;; terms with nested quantification interspersed with sort
;;; declarations.

(defclass abstract-context ()
  ()
  (:documentation
   "Context needed to parse terms with nested scopes."))

(defgeneric enclosing-context (context)
  (:documentation
   "Returns the enclosing context of CONTEXT, or NIL if CONTEXT has no
   enclosing context.")
  (:method ((context abstract-context))
    (declare (ignore context))
    nil))

(defgeneric constants-for-sort-table (context)
  (:documentation
   "Return a table that contains a list of constants for each sort."))

(defgeneric declare-sort-for-constant (constant context)
  (:documentation
   "Declare the sort for CONSTANT in CONTEXT.  If the sort is is NIL
   then don't add a declaration."))

(defgeneric constants-for-sort (sort context)
  (:documentation
   "Return a list of all constants for SORT in CONTEXT.")
  (:method (sort context)
    (gethash sort (constants-for-sort-table context) '())))

(defgeneric declarations (context)
  (:documentation
   "Returns an extensible sequence of all logical declarations (sort
declations, constant and function symbol declarations, logical assertions,
etc. for this context."))

(defgeneric (setf declarations) (new-declarations context)
  (:documentation
   "Sets the logical declarations for CONTEXT to NEW-DECLARATIONS."))

(defgeneric declared-operator-sorts (context)
  (:documentation
   "Returns a hash table mapping operators to their declared sorts in
   CONTEXT."))

(defgeneric (setf declared-operator-sorts) (new-value context)
  (:documentation
   "Sets the hash table mapping operators to their declared sorts in
   CONTEXT."))

(defvar *warn-for-null-operator-sorts* t)

(defgeneric declare-operator-sort (operator sort context)
  (:documentation
   "Declare OPERATOR to have SORT in CONTEXT, i.e., subsequent calls
   to (GETHASH OPERATOR (DECLARED-OPERATOR-SORTS CONTEXT)) should return
   true.")
  (:method (operator sort context)
    (if sort
        (setf (gethash operator (declared-operator-sorts context)) sort)
        (when *warn-for-null-operator-sorts*
          (warn "No sort declaration for operator ~A." operator)))))

(defgeneric terms-with-unique-names (context)
  (:documentation
   "Returns a sequence containing all terms in CONTEXT for which unique names
   axioms should be generated."))

(defgeneric add-to-terms-with-unique-names (term context)
  (:documentation
   "Adds a unique TERM to CONTEXT.  Has no effect if TERM is already a unique
   term for context."))

(defgeneric lookup-functor (name arity context &optional create?)
  (:documentation
   "Return the functor with NAME and ARITY for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh
   functor, if CREATE? is false return NIL."))

(defgeneric (setf lookup-functor) (new-value name arity context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-FUNCTOR for NAME and ARITY."))

(defgeneric variables (context)
  (:documentation
   "Return a list of all variables in CONTEXT and its enclosing contexts.")
  (:method (context)
    (declare (ignore context))
    '()))

(defgeneric lookup-variable (name sort context &optional create?)
  (:documentation
   "Return the variable NAME with sort SORT for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh
   variable, if CREATE? is false return NIL."))

(defgeneric (setf lookup-variable) (new-value name sort context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-VARIABLE for NAME."))

(defgeneric lookup-number (value context &optional create?)
  (:documentation
   "Return a number constant with value VALUE for the given CONTEXT if it
   exists. Otherwise, if CREATE? is true, create and return a fresh number
   constant, if CREATE? is false return NIL."))

(defgeneric (setf lookup-number) (new-number value context)
  (:documentation
   "Assign NEW-VALUE as the new value of LOOKUP-NUMBER for VALUE."))

(defgeneric known-operators (context)
  (:documentation
   "A hash table containing a hash table that maps known operators into their
   term type (represeted as symbol)."))

;;; TODO: Do we need this?  Should we have it?  What should its sematics be
;;; with regards to lexical/dynamic scoping?
(defgeneric (setf known-operators) (new-value context))

(defgeneric the-empty-program-term (context)
  (:documentation
   "Returns an instance of EMPTY-PROGRAM-TERM for CONTEXT that might be interned.")

  (:method ((context abstract-context))
    (cond ((next-method-p)
           (call-next-method))
          (t
           (warn "Generating uninterned empty program term")
           (make-instance 'empty-program-term :context context)))))

(defgeneric the-no-operation-term (context)
  (:documentation
   "Returns an instance of NO-OPERATION-TERM for CONTEXT that might be interned.")

  (:method ((context abstract-context))
    (cond ((next-method-p)
           (call-next-method))
          (t
           (warn "Generating uninterned no-operation term.")
           (make-instance 'no-operation-term :context context)))))

;;; Methods for Obtaining Primitive Actions
;;; ---------------------------------------

;;; The definition of a primitive action is provided by instances of
;;; PRIMITIVE-ACTION.

(defgeneric primitive-actions (context)
  (:documentation
   "A hash table containing the description of each primitive action known in
   CONTEXT."))

;;; TODO: See (setf known-operators).
(defgeneric (setf primitive-actions) (new-value context))

;;; Methods for Obtaining Nature's Choices
;;; --------------------------------------

(defgeneric natures-choices-table (context)
  (:documentation
   "A hash table containing the description of each nature's choice known in
   CONTEXT."))

;;; Methods for Obtaining Sorts
;;; ---------------------------

(defgeneric sorts (context)
  (:documentation
   "A hash table mapping sort names to sorts for CONTEXT."))


;;; Methods for Obtaining Fluents
;;; -----------------------------

(defgeneric fluents (context)
  (:documentation
   "A hash table containing the description of every fluent in CONTEXT."))


;;; TODO: see (setf known-operators)
(defgeneric (setf fluents) (new-value context))


;;; Operator and Context Mixins
;;; ===========================

(defgeneric operator (compound-term)
  (:documentation "The operator of COMPOUND-TERM.")
  (:method (term)
    (declare (ignore term))
    :unknown-operator))

(defclass operator-mixin ()
  ((operator :accessor operator :initarg :operator
             :initform (required-argument :operator)))
  (:documentation "Mixin that provides an OPERATOR slot."))

(defgeneric context (thing)
  (:documentation "The compilation context in which THING is relevant."))

(defclass context-mixin ()
  ((context
    :accessor context :initarg :context 
    :initform (required-argument :context)
    :documentation "The context to which this object belongs."))
  (:documentation "Mixin that provides a CONTEXT slot."))

(defvar *default-context*)
