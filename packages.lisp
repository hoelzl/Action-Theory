;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

(defpackage #:action-theory
  (:use #:common-lisp #:alexandria #:iterate)
  (:import-from #:snark-lisp
                #:forall #:exists #:not #:iff)
  (:nicknames #:at)
  (:export . #.*action-theory-utilities-exports*)
  (:export . #.*action-theory-context-exports*)
  (:export . #.*action-theory-term-exports*)
  (:export . #.*action-theory-operator-exports*)
  (:export . #.*action-theory-situation-exports*)
  (:export . #.*action-theory-parser-exports*)
  (:export . #.*action-theory-snark-exports*))
     
(defpackage #:action-theory-utilities
  (:use #:action-theory)
  (:nicknames #:utils)
  (:export . #.*action-theory-utilities-exports*))

(defpackage #:action-theory-syntax
  ;; The action-theory-syntax package contains the implementation of contexts, and
  ;; action-theory.  We define packages that export subsets of these symbols for use
  ;; by other programs.
  (:use #:action-theory)
  (:nicknames #:syntax)
  (:export . #.*action-theory-context-exports*)
  (:export . #.*action-theory-term-exports*)
  (:export . #.*action-theory-operator-exports*)
  (:export . #.*action-theory-situation-exports*))

(defpackage #:action-theory-context
  (:use #:action-theory)
  (:nicknames #:context)
  (:export . #.*action-theory-context-exports*))

(defpackage #:action-theory-terms
  (:use #:action-theory)
  (:nicknames #:terms)
  (:export . #.*action-theory-term-exports*))

(defpackage #:action-theory-operators
  (:use #:action-theory)
  (:nicknames #:operators)
  (:export . #.*action-theory-operator-exports*))

(defpackage #:action-theory-situation
  (:use #:action-theory)
  (:nicknames #:situation)
  (:export . #.*action-theory-situation-exports*))

(defpackage #:action-theory-parser
  (:use #:action-theory)
  (:nicknames #:parser)
  (:export . #.*action-theory-parser-exports*))

(defpackage #:action-theory-snark
  (:use #:common-lisp #:snark #:iterate
        #:action-theory-utilities #:action-theory-context)
  (:nicknames #:osnark)
  (:import-from #:action-theory
                . #.*action-theory-snark-exports*)
  (:export . #.*action-theory-snark-exports*))

(defpackage #:action-theory-user
  (:use #:common-lisp #:alexandria #:iterate
        #:common-lisp-user
        #:action-theory))

(defpackage #:action-theory-tests
  (:use #:common-lisp #:alexandria #:iterate
	#:action-theory))
