;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Forward Declarations
;;; ====================

(defgeneric termp (thing)
  (:documentation 
   "Return true if THING is a term.")
  (:method (thing)
    (declare (ignore thing))
    nil))

;;; Names
;;; =====

(defclass name-mixin ()
  ((name
    :accessor name :initarg :name :initform :<unnamed> :type symbol
    :documentation "The name of the entity that inherits this mixin."))
  (:documentation
   "Mixin inherited by all classes that have names."))

(defclass required-name-mixin (name-mixin)
  ((name
    :initform (required-argument :name)
    :documentation "The name of the entity that inherits this mixin."))
  (:documentation
   "Mixin inherited by all classes that require a name."))


;;; Errors
;;; ======

;;; Define the ACTION-THEORY-ERROR class here so that all other packages can
;;; derive from it.

(define-condition action-theory-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "An error occurred during action-theory processing."))))

(define-condition invalid-class (action-theory-error)
  ((expected-class :accessor expected-class :initarg :expected-class
                   :initform (required-argument :expected-class))
   (current-class :accessor current-class :initarg :current-class
                  :initform (required-argument :current-class)))
  (:report (lambda (condition stream)
             (format stream "~A is not an instance of ~A."
                     (class-name (current-class condition))
                     (class-name (expected-class condition))))))

(define-condition incompatible-sort-declarations (action-theory-error)
  ((thing :initarg :thing)
   (sort-1 :initarg :sort-1)
   (sort-2 :initarg :sort-2))
  (:report (lambda (condition stream)
             (with-slots (thing sort-1 sort-2) condition
               (format stream "Incompatible sort declarations for ~W: ~W, ~W."
                       thing sort-1 sort-2)))))

;;; Lookup
;;; ======

(defgeneric lookup-table-accessor-for-type (type)
  (:documentation
   "Return a function that can access the table for instances of TYPE in a
   context.")
  (:method (type)
    (error "No lookup-table accessor for ~A." type)))


(define-condition no-declaration-for-domain-element
    (action-theory-error)
  ((name :initarg :name)
   (type :initarg :type)
   (context :initarg :context))
  (:report (lambda (condition stream)
             (with-slots (name type context) condition 
               (format stream "No domain element ~A of type ~A in context ~:W"
                       name type context)))))



(defgeneric lookup (name type context &optional default)
  (:documentation
   "Returns the definition of NAME of type TYPE in CONTEXT.  Signals an error
   if no primitive action exists and no DEFAULT is supplied.")
  (:method ((name symbol) type context
            &optional (default nil default-supplied-p))
    (or (gethash name
                 (funcall (lookup-table-accessor-for-type type) context)
                 nil)
        (if default-supplied-p
            default
            (cerror "Return NIL."
                    'no-declaration-for-domain-element
                    :name name :type type :context context)))))

(defun (setf lookup) (new-value name type context)
  "Set the definition of NAME for TYPE in CONTEXT to NEW-VALUE."
  (setf (gethash name
                 (funcall (lookup-table-accessor-for-type type) context))
        new-value))

;;; Information about the Lisp version
;;; ==================================

(defvar *features-for-lisp-types*
  '(("Clozure Common Lisp" :ccl :ccl-1.8 :clozure :clozure-common-lisp)
    ("SBCL" :sbcl)
    ("CMU Common Lisp" :cmucl :cmu :cmu20)))

(defun feature-for-lisp-type (&optional (lisp-type (lisp-implementation-type)))
  (or (second (assoc lisp-type *features-for-lisp-types* :test #'string-equal))
      (cerror "Return :UNKNOWN-LISP"
              "There is no known feature for ~A." lisp-type)
      :unknown-lisp))

;;; General utilities
;;; =================

(defmacro defglobal (name value &optional doc)
  "Define a global variable.  In Lisps that don't provide lexical global
variables this is identical to a DEFVAR."
  `(#+sbcl sb-ext:defglobal
    #-sbcl defvar
    ,name ,value ,@(if doc (list doc) ())))

(defmacro gethash* (key hash-table default-value)
  "Get a value from a hash table, setting it if it does not already exist.

  For example, if *hash* is the hash table { a => 1 } then
    (gethash* 'a *hash* 10) => 1,  T   and *hash* is unchanged;
    (gethash* 'b *hash* 10) => 10, NIL and *hash* => { a => 1; b => 10 }"
  (once-only (key hash-table)
    `(multiple-value-bind (value key-present-p)
	 (gethash ,key ,hash-table nil)
       (if (not key-present-p)
	   (let ((default ,default-value))
	     (setf (gethash ,key ,hash-table) default)
	     (values default nil))
	   (values value t)))))


(defun unquote (thing)
  "If THING is a list with first element EQL to QUOTE, return the second
  element, otherwise return THING unchanged."
  (if (and (consp thing) (eql (first thing) 'quote))
      (second thing)
      thing))

(defun wrap-in-quote (thing)
  "Create a list of the form `(QUOTE ,THING) unless THING is NIL, a number, a
  keyword or a string."
  (if (or (null thing) (numberp thing) (keywordp thing) (stringp thing))
      thing
      (list 'quote thing)))

(defun wrap-in-forall (variables term)
  "Create a list of the form `(FORALL ,VARIABLES ,TERM) unless VARIABLES is
  empty, in which case TERM is returned unchanged."
  (if variables
      (list 'forall variables term)
      term))

(defun wrap-in-exists (variables term)
  "Create a list of the form `(EXISTS ,VARIABLES ,TERM) unless VARIABLES is
  empty, in which case TERM is returned unchanged."
  (if variables
      (list 'exists variables term)
      term))

(defun sexpr-equal-p (x y &optional (symbol-map (make-hash-table)))
  "This function is based on CCL's definition of EQUALP, and modified to be
  useful for approximate comparison of sexprs for terms.  If X and Y are
  interned symbols they are regarded as equal and stored in the symbol-map if
  they do not already appear in the symbol map as different symbols.  If X and
  Y are uninterned symbols, they are regarded as equal when their SYMBOL-NAMEs
  are STRING-EQUAL."
  (cond ((eql x y) t)
        ((and (symbolp x) (symbolp y))
         (if (not (symbol-package x))
             (and (not (symbol-package y))
                  (string-equal (symbol-name x) (symbol-name y)))
             (if-let (x-val (gethash x symbol-map))
               (if (eq x-val y) t nil)
               (if-let (y-val (gethash y symbol-map))
                 (if (eq y-val x) t nil)
                 (progn (setf (gethash x symbol-map) y
                              (gethash y symbol-map) x)
                        t)))))
        ((characterp x) (and (characterp y) (eq (char-upcase x) (char-upcase y))))
        ((numberp x) (and (numberp y) (= x y)))
        ((consp x)
         (and (consp y)
              (sexpr-equal-p (car x) (car y) symbol-map)
              (sexpr-equal-p (cdr x) (cdr y) symbol-map)))
        ((pathnamep x) (equal x y))
        ((vectorp x)
         (and (vectorp y)
              (let ((length (length x)))
                (when (eq length (length y))
                  (dotimes (i length t)
                    (declare (fixnum i))
                    (let ((x-el (aref x i))
                          (y-el (aref y i)))
                      ;; TODO: Why don't we use SEXPR-EQUAL-P here?
                      (unless (or (eq x-el y-el) (equalp x-el y-el))
                        (return nil))))))))
        ((arrayp x)
         (error "SEXPR-EQUAL-P for arrays not currently implemented."))
        ;; TODO: Is this specialization on snark-variables really necessary?
        ;; Can't we just use EQL or EQUAL for structures (and hash-tables and
        ;; random-states)?
        ((and (typep x 'snark::variable) (typep y 'snark::variable))
         (eq x y))
        ((and (typep x 'structure-object) (typep y 'structure-object))
         (error "SEXPR-EQUAL-P for general structures not currently implemented."))
        ((and (hash-table-p x) (hash-table-p y))
         (error "SEXPR-EQUAL-P for hash tables not currently implemented."))
	((and (random-state-p x) (random-state-p y))
         (error "SEXPR-EQUAL-P for random states not currently implemented"))
        (t nil)))

(defun random-hex-list (length)
  "Return a list of the given LENGTH containing randum numbers between 0 and
  15 (inclusive)"
  (if (<= length 0)
      '()
      (cons (random 16) (random-hex-list (1- length)))))

(defun make-uuid (&optional (stream nil))
  "A simple (inefficient and probably incorrect) implementation of type 4
UUIDs."
  (format stream
          "~{~X~}-~{~X~}-4~{~X~}-A~{~X~}-~{~X~}"
          (random-hex-list 8) (random-hex-list 4)
          (random-hex-list 3) (random-hex-list 3)
          (random-hex-list 12)))

(defun make-uuid-symbol (&optional (package (find-package '#:keyword)))
  "Return a new symbol whose SYMBOL-NAME is a type 4 UUID.  If no PACKAGE is
  given, the result is interned in the KEYWORD package, otherwise in PACKAGE."
  (intern (make-uuid) package))

;;; Helper Methods for Macro Definitions
;;; ====================================

(defun uncons (thing)
  "Return the first element of THING if THING is a cons, THING otherwise."
  (if (consp thing)
      (first thing)
      thing))

(defun extract-arguments-from-lambda-list (lambda-list)
  "Extract the names of parameters and keywords from LAMBDA-LIST so that the
  result is an arglist suitable for calling a delegate with a lambda list of
  the given form.  Reports and error if LAMBDA-LIST has a &rest parameter.
  For example, if LAMBDA-LIST is (TERM &KEY REASON (ERRORP T)), the result is
  (TERM :REASON REASON :ERRORP ERRORP)"
  (multiple-value-bind (required optional rest keys)
      (parse-ordinary-lambda-list lambda-list :allow-specializers t)
    (assert (not rest) ()
            "Cannot currently handle lambda-lists with &REST parameter.")
    `(,@(mapcar 'uncons required) ,@(mapcar 'first optional) ,@(mapcan 'first keys))))

;;; Three-Valued Logic
;;; ==================

;;; There are many properties of programs that cannot be decided
;;; during compile time.  To handle them in a uniform manner we
;;; introduce a three-valued logic.

(deftype boolean3 ()
  '(member t :unknown nil))

(defun and3 (&rest args)
  (let ((top-result t))
    (mapc (lambda (arg)
            (cond ((not arg)
                   (return-from and3 nil))
                  ((eql arg :unknown)
                   (setf top-result :unknown))))
          args)
    top-result))

(defun or3 (&rest args)
  (let ((bottom-result nil))
    (mapc (lambda (arg)
            (cond ((eql arg :unknown)
                   (setf bottom-result :unknown))
                  (arg
                   (return-from or3 t))))
          args)
    bottom-result))

