;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defgeneric starts-with-question-mark-p (name)
  (:documentation
   "Returns true if NAME is a string or symbol of length > 1 that starts with
   a question mark.")
  (:method ((name symbol))
    (starts-with-question-mark-p (symbol-name name)))
  (:method ((name string))
    (and (> (length name) 1)
	 (eql (char name 0) #\?))))

;;; Handling of Declarations
;;; ========================

#+(or)
(defgeneric process-declaration-for-parsing (declaration context)
  (:documentation
   "Process a declaration so that the parser can use it for processing
   the rest of the program.")

  (:method ((term term) context)
    (declare (ignore term context))
    :do-nothing)

  (:method :after ((term term-with-unique-name-mixin) context)
    (add-to-terms-with-unique-names term context))

  (:method ((term constant-declaration-term) context)
    (declare-sort-for-constant term context))

  ;; TODO: We might want to do something along the following lines.  For now
  ;; we use a simple implementation that always calls DEFINE-PRIMITIVE-ACTION.
  #+(or)
  (:method ((declaration primitive-action-declaration-term) context)
    (with-slots (name) declaration
      (let ((class (find-class name)))
        (if class
            (cond ((not (typep class 'primitive-action))
                   (cerror "Redefine the class."
                           'invalid-class-for-action-theory-element
                           :expected-class 'primitive-action
                           :current-class class)
                   (define-primitive-action (name declaration)
                       (declared-sort declaration context)
                     :precondition (precondition declaration)))
                  ;; TODO: Handle case of signature changes.
                  (t
                   (define-primitive-action (name declaration)
                       (declared-sort declaration)
                     :precondition (precondition declaration))))))))
  (:method ((declaration arity-declaration-term) context)
    (declare-operator-sort (name declaration)
                           (declared-sort declaration context)
                           context))

  (:method ((declaration primitive-action-declaration-term) context)
    (declare-operator-sort (name declaration)
                           (declared-sort declaration context)
                           context)
    (declare-primitive-action (name declaration) context))

  (:method ((declaration functional-fluent-declaration-term) context)
    (declare-operator-sort (name declaration)
                           (declared-sort declaration context)
                           context)
    (apply #'define-functional-fluent (name declaration)
           (signature declaration)
           (keywords declaration))
    (declare-functional-fluent (name declaration) context))

  (:method ((declaration relational-fluent-declaration-term) context)
    (declare-operator-sort (name declaration)
                           (declared-sort declaration context)
                           context)
    (apply #'define-relational-fluent (name declaration)
           (signature declaration)
           (keywords declaration))
    (declare-relational-fluent (name declaration) context)))


;;; Defgeneric form for PARSE-INTO-TERM-REPRESENTATION is in syntax.lisp.

(defgeneric parse-arguments-for-term (term arguments context)
  (:documentation
   "Parse the argument list for a TERM-instance. ARGUMENTS is the
   argument list without the leading symbol that determines TERM's
   type."))

(defmethod parse-arguments-for-term ((term application-term) arguments context)
  "Parse each argument for an application term into term representation in CONTEXT."
  (setf (arguments term)
        (mapcar (lambda (subexp)
                  (parse-into-term-representation (unquote subexp) context))
                arguments)))

(defmethod parse-arguments-for-term ((term test-term) arguments context)
  (declare (ignore context))  
  (setf (argument term)
        (parse-into-term-representation (unquote (first arguments))
                                        (context term)))
  (let ((keywords (mapcar 'unquote (rest arguments))))
    (setf (keywords term) keywords)))

(defmethod parse-arguments-for-term ((term body-term) arguments context)
  "Parse each for a body term argument into term representation in CONTEXT."
  (setf (body term)
        (mapcar (lambda (subexp)
                  (parse-into-term-representation subexp context))
                arguments)))

(defmethod parse-arguments-for-term ((term cases-term) arguments context)
  (setf (sdp-cases term)
        (mapcar (lambda (spec)
                  (destructuring-bind (&key case value alias) spec
                    (make-instance 'sdp-case
                      :case-term (parse-into-term-representation
                                  case context)
                      :value value
                      :alias alias)))
                arguments)))

(defgeneric parse-binding (term binding-list context)
  (:documentation
   "Parse the BINDING-LIST for a single binding of TERM in CONTEXT.
BINDING-LIST may be a singleton list containing a representation of the bound
variable, or a list consisting of a representation of the bound variable
followed by a property list of keyword-value pairs."))

(defmethod parse-binding (binding (term binding-term) context)
  "Default implementation for PARSE-BINDING, parsing the variable and
accepting all keyword arguments without parsing any of them."
  (let ((binding-variable (parse-variable-term binding context)))
    (check-type binding-variable variable-term)
    (setf (is-bound-p binding-variable) t)
    binding-variable))

(defmethod parse-arguments-for-term :around ((term binding-term) arguments context)
  "Parse the binding list and then call the next method on the rest of the
argument list."
  (let* ((binding-list (ensure-list (first arguments)))
         (bound-variables (mapcar (lambda (binding)
                                    (parse-binding binding term context))
                                  binding-list)))
    (setf (bound-variables term) bound-variables)
    (call-next-method term (rest arguments) context)))

(defmethod parse-arguments-for-term :after ((term declaration-term) arguments context)
  "Add TERM to the declarations of CONTEXT.  This has to happen after the
arguments are passed, otherwise the name of TERM will not be set."
  (declare (ignore arguments))
  (vector-push-extend term (declarations context))
  #+(or)
  (process-declaration-for-parsing term context))


;; TODO: We currently simply unquote arguments that should actually be
;; evaluated by the interpreter.  This is so that we don't have to fix all
;; examples when the interpreter is complete enough.
(defmethod parse-arguments-for-term ((term named-declaration-term) arguments context)
  (declare (ignore context))
  (setf (name term) (unquote (first arguments)))
  (setf (keywords term) (mapcar 'unquote (rest arguments))))

(defmethod parse-arguments-for-term ((term subsort-declaration-term) arguments context)
  (declare (ignore context))  
  (setf (name term) (unquote (first arguments)))
  (setf (supersort term) (unquote (second arguments)))
  (setf (keywords term) (mapcar 'unquote (cddr arguments))))

(defmethod parse-arguments-for-term
    ((term sorts-incompatible-declaration-term) arguments context)
  (declare (ignore context))
  (setf (sorts term) (mapcar 'unquote arguments)))

(defmethod parse-arguments-for-term ((term arity-declaration-term) arguments context)
  (declare (ignore context))
  (setf (name term) (unquote (first arguments)))
  (setf (arity term) (unquote (second arguments)))
  (setf (keywords term) (mapcar 'unquote (cddr arguments))))

(defmethod parse-arguments-for-term ((term signature-declaration-term) arguments context)
  (declare (ignore context))
  (setf (name term) (unquote (first arguments)))
  (setf (signature term) (unquote (second arguments)))
  (setf (keywords term) (mapcar 'unquote (cddr arguments))))

(defmethod parse-arguments-for-term
    ((term ordering-declaration-term) arguments context)
  (declare (ignore context))
  (setf (ordered-symbols term) (mapcar 'unquote arguments)))

(defmethod parse-arguments-for-term
    ((term logical-sentence-declaration-term) arguments context)
  (setf (sentence term) 
        (parse-into-term-representation (unquote (first arguments)) context))
  (setf (keywords term) (mapcar 'unquote (rest arguments))))

(defgeneric parse-variable-term (exp context)
  (:documentation
   "Parse EXP as a VARIABLE-TERM.")

  (:method ((exp symbol) (context abstract-context))
    (multiple-value-bind (name sort)
        (destructure-variable-name exp)
      (make-variable-term name sort context)))

  (:method ((exp cons) (context abstract-context))
    (destructuring-bind (name &key sort global &allow-other-keys) exp
      (let ((var (parse-variable-term name context)))
        (when global
          (setf (global var) global))
        (when sort
          (if (and (declared-sort var context)
                   (not (eql (declared-sort var context) t))
                   (not (eql (declared-sort var context) sort)))
              (cerror "Ignore the explicit sort declaration."
                      'incompatible-sort-declarations
                      :thing exp (declared-sort var context) sort)
              (setf (declared-sort var context) sort)))
        var)))

  (:method ((exp variable-term) (context abstract-context))
    (setf (context exp) context)
    exp))


(defun parse-term (expression
                   &optional (context (make-instance 'local-context
                                        :enclosing-context *default-context*)))
  "A simple wrapper around PARSE-INTO-TERM-REPRESENTATION that provides a
  fresh default context.  Mainly for interactive exploration."
  (parse-into-term-representation expression context))

(defgeneric parse-into-term-representation (expression context)
  (:documentation
   "Parse EXPRESSION into term representation in CONTEXT."))

(defmethod parse-into-term-representation ((term term) (context abstract-context))
  "When re-parsing an already parsed term, return it unchanged.  But only if
the new context equals the one in which it was originally parsed."
  (assert (eql (context term) context) ()
          "Cannot parse term ~W in context ~W."
          term context)
  term)

;;; TODO: Add support for true and false.
;;;
(defmethod parse-into-term-representation ((exp symbol) (context abstract-context))
  "Parse a single symbol.
  If it is NIL or NULL return an empty program term.
  If it starts with a question mark, make a variable for CONTEXT.
  If neither of these cases apply, return a primitive term."
  (cond ((or (eql exp 'nil) (eql exp 'null))
	 (make-instance 'empty-program-term :context context :source exp))
	((starts-with-question-mark-p exp)
	 (parse-variable-term exp context))
	(t
         (multiple-value-bind (name sort)
             (destructure-variable-name exp)
           (or (lookup-variable name sort context nil)
               (make-instance 'primitive-term
                 :value exp :context context :source exp))))))

(defmethod parse-into-term-representation ((exp number) (context abstract-context))
  "Return a number term with value EXP."
  (make-instance 'number-term :value exp :context context))

(defmethod parse-into-term-representation ((exp string) (context abstract-context))
  "Return a primitive term with value EXP."
  (make-instance 'primitive-term :value exp :context context))

(defmethod parse-into-term-representation
    ((exp snark::variable) (context abstract-context))
  "Return a variable term corresponding to EXP in the current context."
  (let* ((sort (snark-feature::feature-name (snark::variable-sort exp)))
         (name (make-symbol (format nil "?SV~A.~A"
                                    (snark::variable-number exp)
                                    sort))))
    (make-instance 'variable-term :name name :unique-name name
                   :sort sort
                   :context context
                   :intern nil :is-bound-p nil)))

(defmethod parse-into-term-representation ((exp cons) (context abstract-context))
  "Return a TERM-instance for EXP in CONTEXT.
  If the operator of EXP has a known term type make an instance of that type.
  Otherwise, if the operator is a primitive action, then return an instance of
  that action's class.
  Otherwise, if the operator is a fluent return an instance of the fluent's
  class.
  Otherwise return an instance of UNKNOWN-GENERAL-APPLICATION-TERM."
  (let* ((operator (first exp))
	 (known-type (term-type-for-operator operator context nil))
         (term (cond (known-type
                      (apply 'make-instance known-type :context context :source exp '()))
                     ((let ((primitive-action
                              (gethash operator (primitive-actions context) nil)))
                        (if primitive-action
                            (make-instance 'primitive-action-term
                                           :context context :source exp
                                           :primitive-action primitive-action)
                            nil)))
                     ((let ((fluent
                              (gethash operator (fluents context) nil)))
                        (if fluent
                            (make-instance 'fluent-term
                              :fluent fluent :context context :source exp)
                            nil)))
                     (t
                      (make-instance 'unknown-general-application-term
                                     :operator operator :context context :source exp)))))
    (parse-arguments-for-term term (rest exp) (context term))
    term))
