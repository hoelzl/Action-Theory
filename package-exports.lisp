;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
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
      ;; Macro helpers
      #:uncons #:extract-arguments-from-lambda-list
      ;; Three-valued logic
      #:boolean3
      #:and3 #:or3 #:not3
      ;; Macros
      #:extract-arguments-from-lambda-list
      #:defdelegate #:define-delegates
      #:define-interning-make-instance
      #:maybe-suppress-snark-output

      ;; Double exports (from syntax) for Snark
      #:to-sexpr
      #:negate #:universally-quantify #:existentially-quantify
      #:set-up-snark))
  
  (defvar *action-theory-context-exports*
    '(;; Forward declaration
      #:context
      #:primitive-action
      #:precondition #:natures-choices
      #:lookup
      #:declare-primitive-action
      #:fluent
      #:successor-state-axiom
      #:declare-fluent
      #:is-relational-fluent-p
      #:is-functional-fluent-p
      #:arguments-mixin #:arguments
      #:known-term #:is-known-term-p
      #:natures-choice
      #:declare-natures-choice

      #:reward-function
      #:reward-cases
      #:declare-reward-function

      #:sdp-case
      #:case-term #:value #:alias
      #:cases-term
      #:sdp-cases
      #:disjoin-cases #:sort-cases #:partition-cases
      #:add-cases #:multiply-cases #:casemax
      #:existentially-quantify-cases

      #:abstract-context
      #:declarations
      #:declared-operator-sorts #:declare-operator-sort
      #:terms-with-unique-names #:add-to-terms-with-unique-names
      #:lookup-functor #:lookup-variable #:lookup-number
      #:known-operators #:default-known-operators
      #:primitive-actions #:default-primitive-action-names
      #:fluents
      #:the-empty-program-term #:the-no-operation-term
      #:context-mixin #:context
      #:singleton-terms-mixin
      #:terms-with-unique-names-mixin
      #:compilation-unit
      #:local-context #:enclosing-context #:local-variables
      #:top-level-context #:*default-context*))
  
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

      #:empty-program-term
      #:is-final-term-p
      #:primitive-action-term
      #:precondition-term
      #:no-operation-term #:no-operation
      #:test-term
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
      #:term-with-unique-name-mixin
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
      #:cases
      
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
      #:parse-into-term-representation
      #:parse-term))
  
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

