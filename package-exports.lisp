;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

;;; TODO: These exports are out of date.  Update the export lists and
;;; sanitize the package structure.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *action-theory-utilities-exports*
    '(;; Names
      #:name-mixin #:required-name-mixin #:name
      ;; Errors
      #:action-theory-error
      #:invalid-class
      #:incompatible-sort-declarations
      ;; Lisp implementation
      #:*features-for-lisp-types*
      #:feature-for-lisp-type
      ;; General utilities
      #:unquote
      #:wrap-in-quote #:wrap-in-forall #:wrap-in-exists
      #:sexpr-equal-p
      #:defglobal
      #:gethash*
      #:make-uuid #:make-uuid-symbol
      ;; Macros
      #:extract-arguments-from-lambda-list
      #:defdelegate #:define-delegates
      #:define-interning-make-instance
      #:maybe-suppress-snark-output
      ;; Support for testing
      #:action-theory-suite
      #:action-theory-utilities-suite
      #:action-theory-macro-suite
      #:action-theory-syntax-suite
      #:action-theory-situation-suite
      #:action-theory-parser-suite
      #:action-theory-interpreter-suite
      #:action-theory-compiler-suite
      #:action-theory-builtins-suite

      ;; Double exports (from syntax) for Snark
      #:to-sexpr
      #:negate #:universally-quantify #:existentially-quantify
      #:set-up-snark))

  (defvar *action-theory-context-exports*
    '(;; Forward declaration
      #:parse-into-term-representation
      
      #:primitive-action-definition #:context
      #:action-class #:action-precondition
      #:declare-primitive-action
      #:fluent-definition
      #:fluent-class #:fluent-successor-state-axiom
      #:relational-fluent-definition
      #:declare-relational-fluent
      #:functional-fluent-definition
      #:declare-functional-fluent
      #:arguments-mixin #:arguments
      #:known-term #:is-known-term-p
      
      #:compilation-context
      #:declarations
      #:unique-terms #:add-unique-term
      #:lookup-functor #:lookup-variable #:lookup-number
      #:primitive-actions #:default-primitive-action-names
      #:fluents
      #:the-empty-program-term #:the-no-operation-term
      #:context-mixin #:context
      #:singleton-terms-mixin
      #:unique-terms-mixin
      #:compilation-unit
      #:local-context #:enclosing-context #:local-variables))
  
  (defvar *action-theory-term-exports*
    '(#:term #:source
      #:variable-term #:unique-name #:variable-sort #:is-bound-p
      #:make-unique-variable-name
      #:make-variable-term 
      #:make-anonymous-variable-term 
      #:atomic-term
      #:primitive-term #:value
      #:functor-term
      #:number-term
      #:compound-term #:is-compound-term #:operator
      #:term-type-for-operator
      #:application-term #:arguments
      #:unknown-general-application-term
      #:known-compound-term 
      #:known-application-term
      #:unary-term #:argument
      #:binary-term #:lhs #:rhs
      #:ternary-term #:arg1 #:arg2 #:arg3
      #:known-general-application-term
      #:body-term #:body
      #:binding-term #:bound-variables
      
      #:conjunction-term
      #:disjunction-term
      #:negation-term
      #:implication-term
      #:reverse-implication-term
      #:equivalence-term
      #:quantification-term
      #:universal-quantification-term
      #:existential-quantification-term

      #:*default-max-solution-depth*
      #:multi-solution-mixin
      #:soulution-depth #:max-solution-depth
      #:clone-multi-solution-term-increasing-depth

      #:empty-program-term
      #:is-final-term-p
      #:primitive-action-term
      #:precondition-term
      #:no-operation-term #:no-operation
      #:test-term
      #:solution-depth #:max-solution-depth
      #:sequence-term
      #:action-choice-term
      #:argument-choice-term
      #:iteration-term
      #:conditional-term
      #:while-loop-term
      #:search-term
      #:concurrent-term
      #:prioritized-concurrent-term
      #:spawn-term

      #:arity
      #:declaration-term
      #:keywords-mixin #:keywords
      #:local-context-mixin
      #:unique-term-mixin
      #:declared-sort #:successor-state-axiom
      #:named-declaration-term
      #:sort-declaration-term
      #:subsort-declaration-term #:supersort
      #:sorts-incompatible-declaration-term #:sorts
      #:signature-declaration-term #:signature
      #:primitive-action-declaration-term
      #:precondition
      #:functional-fluent-declaration-term
      #:relational-fluent-declaration-term
      #:constant-declaration-term
      #:unique-constant-declaration-term
      #:arity-declaration-term
      #:function-declaration-term
      #:unique-function-declaration-term
      #:relation-declaration-term
      #:ordering-declaration-term #:ordered-symbols

      #:logical-sentence-declaration-term
      #:sentence
      #:logical-assertion-term
      #:logical-assumption-term
      #:rewrite-assertion-term
      
      #:definition-term
      #:primitive-action-declaration-term
      #:procedure-definition-term
      #:domain-definition-term
      
      #:to-sexpr
      #:free-variables #:free-variable-sexprs
      #:contains-variable-p
      #:substitute-term #:substitute-terms
      #:negate #:universally-quantify
      
      #:variables-and-term-for-universal-quantification
      #:make-unique-names-axiom
      #:make-unique-names-axiom-for-arguments
      #:make-unique-names-axioms

      #:invalid-declaration-type
      #:process-declaration-for-snark
      #:set-up-snark))
  
  (defvar *action-theory-operator-exports*
    '("&" "," ";" "~" "->" "=>" "<-" "<=" "<->" "<=>"
      #:and #:or #:not #:implies
      #:implied-by #:is-implied-by
      #:iff #:equiv #:equivalent #:is-equivalent #:are-equivalent
      #:foreach #:each #:forall #:exist #:exists
      
      "?"
      #:test #:holds #:holds?
      #:seq #:sequentially #:begin #:progn
      #:one-of #:choose #:choose-action
      #:pick #:pick-argument #:choose-argument
      #:repeat #:loop #:iterate
      #:if #:while #:search #:offline
      #:concurrently #:in-parallel
      #:prioritized #:when-blocked
      #:spawn #:new-process

      #:declare-sort #:declare-subsort
      #:declare-sorts-incompatible
      #:declare-ordering-greaterp
      #:declare-constant
      #:declare-unique-constant #:declare-unique-name
      #:declare-function
      #:declare-unique-function
      #:declare-relation
      #:declare-unique-relation
      #:assert #:assert-rewrite
      #:assume
      
      #:poss
      
      #:defaction #:defprimitive
      #:primitive-action #:primact
      #:define-procedure #:defprocedure #:defproc
      #:procedure #:proc
      #:define-domain #:defdomain))
  
  (defvar *action-theory-situation-exports*
    '(#:situation
      #:initial-situation #:s0
      #:successor-situation
      #:next-situation
      #:in-situation))
  
  (defvar *action-theory-parser-exports*
    '(#:starts-with-question-mark-p
      #:process-declaration-for-parsing
      #:parse-arguments-for-term
      #:parse-binding
      #:destructure-variable-name
      #:parse-variable-term
      #:parse-into-term-representation))
  
  (defvar *action-theory-snark-exports*
    '(#:initialize-snark
      #:set-up-theory
      #:*print-snark-output*
      #:prove-or-refute
      #:ida-prove-or-refute
      #:snark-answer
      #:compute-closure
      #:prove-using-snark-depth-zero
      #:prove-using-snark-closure
      #:prove-using-snark))

  ) ; eval-when
