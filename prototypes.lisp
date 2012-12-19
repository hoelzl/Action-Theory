;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defclass prototype-mixin ()
  ((prototype
    :accessor prototype :initarg :prototype
    :initform (required-argument :prototype)
    :documentation "The prototype of this term in the form (NAME . ARGS).")))

(defun nested-context-with-prototype-variables (context prototype)
  (let ((new-context (make-instance 'local-context
				    :enclosing-context context))
	(variables (rest prototype)))
    (mapc (lambda (var) (parse-variable-term var new-context))
	  variables)
    new-context))
    

