;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)
#+debug-action-theory
(declaim (optimize (debug 3) (space 1) (speed 0) (compilation-speed 0)))

(defclass sdp-case ()
  ((case-term :accessor case-term :initarg :case-term)
   (value :accessor value :initarg :value)
   (alias :initarg :alias :initform nil)))

(defun alias (term)
  (or (slot-value term 'alias)
      (case-term term)))

(defun (setf alias) (new-value term)
  (setf (slot-value term 'alias) new-value))

(defmethod print-object ((self sdp-case) stream)
  (with-slots (case-term value alias) self
    (format stream "(:case ~:W ~_:value ~A~:[~; ~_:alias ~:*~:W~])"
	    (to-sexpr case-term) value alias)))
	  

(defclass cases-term (known-application-term)
  ((sdp-cases :accessor sdp-cases :initarg :sdp-cases
	      :initform '()))
  (:documentation
   "Representation of a case statement."))

(defmethod operator ((term cases-term))
  'cases)

(defmethod print-object ((self cases-term) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~:W" (sdp-cases self))))

(defun disjoin-cases (cases-1 cases-2)
  "Returns a new CASES-TERM that contains the disjunction of the SDP-CASES of
  CASES-1 and CASES-2."
  (assert (eql (top-level-context (context cases-1))
               (top-level-context (context cases-2)))
          ()
          "~A and ~A share no common top-level context."
          cases-1 cases-2)
  (make-instance 'cases-term
    :context (top-level-context (context cases-1))
    :sdp-cases (append (sdp-cases cases-1)
                       (sdp-cases cases-2))))

(defun sort-cases (cases &key (test #'>))
  "Destructively sorts the SDP-CASES slot of CASES according to the VALUE of
  each case."
  (setf (sdp-cases cases)
        (sort (copy-list (sdp-cases cases)) test :key #'value))
  cases)

(defun perform-cases-operation (op cases-1 cases-2)
  (make-instance 'cases-term
    :context (top-level-context (context cases-1))
    :sdp-cases (map-product
                (lambda (case-1 case-2)
                  (make-instance 'sdp-case
                    :case-term (make-conjunction (case-term case-1)
                                                 (case-term case-2))
                    :alias `(,op ,(alias case-1) ,(alias case-2))
                    :value (funcall op (value case-1) (value case-2))))
                (sdp-cases cases-1)
                (sdp-cases cases-2))))

(defun add-cases (cases-1 cases-2)
  (perform-cases-operation '+ cases-1 cases-2))

(defun multiply-cases (cases-1 cases-2)
  (perform-cases-operation '* cases-1 cases-2))

(defun existentially-quantify-cases (cases)
  "Returns a new CASES-TERM in which all CASE-TERMs of SDP-CASES are
  existentially quantified."
  (make-instance 'cases-term
    :context (context cases)
    :sdp-cases (mapcar (lambda (sdp-case)
                         (make-instance 'sdp-case
                           :case-term (existentially-quantify
                                       (case-term sdp-case) (context cases))
                           :alias `(existentially-quantify ,(alias sdp-case))
                           :value (value sdp-case)))
                       (sdp-cases cases))))

(defun partition-cases (cases)
  "Returns a partition of CASES with respect to the existing order of
  SDP-CASES."
  (let* ((sdp-cases (sdp-cases cases))
         (context (context cases))
         (initial-case (first sdp-cases))
         (negation-term (negate (case-term initial-case) context))
         (negation-alias `((not ,(alias initial-case))))
         (remaining-cases (rest sdp-cases)))
    (make-instance 'cases-term
      :context context
      :sdp-cases (cons initial-case
                       (mapcar (lambda (case)
                                 (prog1
                                     (make-instance 'sdp-case
                                       :case-term (make-conjunction (case-term case) negation-term)
                                       :alias `(and ,(alias case) ,@negation-alias)
                                       :value (value case))
                                   (setf negation-term (make-conjunction (negate (case-term case) context) negation-term)
                                         negation-alias (list* `(not ,(alias case)) negation-alias))))
                               remaining-cases)))))

(defun casemax (cases)
  "Returns a maximizing partition of CASES with respect to VALUE"
  (partition-cases (sort-cases cases)))

                                                                
