;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package :common-lisp-user)

(defpackage #:action-theory-test
  (:use #:closer-common-lisp #:alexandria #:iterate
	#:action-theory #:hu.dwim.stefil)
  (:nicknames #:at-test #:test)
  (:export #:action-theory-suite
	   #:action-theory-utilities-suite
	   #:action-theory-macro-suite
	   #:action-theory-syntax-suite
	   #:action-theory-parser-suite
	   #:action-theory-situation-suite
	   #:action-theory-snark-suite
	   #:action-theory-substitution-suite))
