;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

;;; Definitions for Nature's Choice
;;; ===============================

(defmethod lookup-table-accessor-for-type ((type (eql 'natures-choice)))
  'natures-choice-table)

(defclass natures-choice (context-mixin prototype-mixin)
  ((probabilities
    :accessor probabilities :initarg :probabilities
    :initform '()
    :documentation
    "A function from state to probability for this nature's choice.")))

(defmethod initialize-instance :after
    ((self natures-choice) &key context prototype probabilities)
  (assert context (context)
          "Cannot create a nature's choice definition without context.")
  (setf (lookup (operator self) 'natures-choice context) self)
  (when (and probabilities (not (termp probabilities)))
    (let* ((new-context (nested-context-with-prototype-variables
                         context prototype))
           (probabilities-term (parse-into-term-representation
				probabilities new-context)))
      (setf (slot-value self 'probabilities) probabilities-term))))

(defmethod print-object ((self natures-choice) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (operator self))))

(defun declare-natures-choice (&key prototype probabilities
				    (context *default-context*))
  (make-instance 'natures-choice
		 :prototype prototype
		 :probabilities probabilities
		 :context context))
