;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defmethod lookup-table-accessor-for-type ((type (eql 'reward-function)))
  'reward-function-table)

(defclass reward-function (name-mixin context-mixin)
  ((reward-cases :accessor reward-cases :initarg :cases
		 :initform '())))

(defmethod initialize-instance :after ((self reward-function)
                                       &key name cases context)
  (assert context (context)
	  "Cannot create a reward function without context.")
  (when name
    (setf (lookup name 'reward-function context) self))
  (when (and cases (not (termp cases)))
    (setf (slot-value self 'reward-cases) 
          (parse-into-term-representation cases context))))
  
(defmethod print-object ((self reward-function) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~A" (name self))))

(defmethod declare-reward-function (&key name cases
                                         (context *default-context*))
  (make-instance 'reward-function
    :name name
    :cases cases
    :context context))
