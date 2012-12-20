;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory-test)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Testing
;;; =======

(eval-when (:compile-toplevel :load-toplevel :execute)

(unless (find-test 'action-theory-suite :otherwise nil)
  (defsuite (action-theory-suite
             :documentation
             "The suite containing all tests for Action-Theory.")))

(unless (find-test 'action-theory-utilities-suite :otherwise nil)
  (defsuite (action-theory-utilities-suite
             :documentation "Tests for utilities."
             :in action-theory-suite)))

(unless (find-test 'action-theory-macro-suite :otherwise nil)
  (defsuite (action-theory-macro-suite
             :documentation "Tests for macros."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-syntax-suite :otherwise nil)
  (defsuite (action-theory-syntax-suite
             :documentation "Tests for the syntax representation."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-parser-suite :otherwise nil)
  (defsuite (action-theory-parser-suite
             :documentation "Tests for the parser."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-situation-suite :otherwise nil)
  (defsuite (action-theory-situation-suite
             :documentation "Tests for the situations."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-snark-suite :otherwise nil)
  (defsuite (action-theory-snark-suite
             :documentation "Tests for the Snark interface."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-substitution-suite :otherwise nil)
  (defsuite (action-theory-substitution-suite
             :documentation "Tests for substitutions."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-continuation-suite :otherwise nil)
  (defsuite (action-theory-continuation-suite
             :documentation "Tests for continuations."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-world-suite :otherwise nil)
  (defsuite (action-theory-world-suite
             :documentation "Tests for the worlds abstraction."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-interpreter-suite :otherwise nil)
  (defsuite (action-theory-interpreter-suite
             :documentation "Tests for the interpreter."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-compiler-suite :otherwise nil)
  (defsuite (action-theory-compiler-suite
             :documentation "Tests for the compiler."
             :in action-theory-suite)))
      
(unless (find-test 'action-theory-builtins-suite :otherwise nil)
  (defsuite (action-theory-builtins-suite
             :documentation "Tests for the built-in predicates."
             :in action-theory-suite)))

) ; eval-when
