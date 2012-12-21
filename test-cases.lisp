;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; 
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory-test)

(in-suite action-theory-cases-suite)

(defun define-domain-for-tests (context)
  (declare-sort :name 'box :context context)
  (declare-sort :name 'city :context context)
  (declare-sort :name 'truck :context context)
  
  #+(or)
  (declare-constant :name 'rome :sort 'city :context context)
  #+(or)
  (declare-constant :name 'paris :sort 'city :context context)
  
  (declare-natures-choice
   :prototype '(load-succeed box.box truck.truck)
   :probabilities '(cases (:case true :value 0.99))
   :context context)

  (declare-natures-choice
   :prototype '(load-fail box.box truck.truck)
   :probabilities '(cases (:case true :value 0.01))
   :context context)

  (declare-primitive-action
   :prototype '(load box.box truck.truck)
   :precondition '(exists (city.city)
		   (and (box-in box city)
		    (truck-in truck city)))
   :natures-choices '(load-succeed load-fail)
   :context context)

  (declare-fluent
   :prototype '(truck-in truck.truck city.city)
   :sort 'boolean
   :successor-state-axiom '(or (and (truck-in truck city)
				(forall (dest.city)
				 (not (and (not (= city dest))
				       (= action (drive-succeed truck dest))))))
			    (= action (drive-succeed truck city)))
   :context context)
  
  (declare-reward-function
   :name 'a-box-in-rome
   :cases '(cases 
	    (:case (exists (box.box)
			   (box-in box rome))
	      :alias a-box-in-rome-partition-1
	      :value 2)
	    (:case (not (exists (box.box)
				(box-in box rome)))
	      :alias a-box-in-rome-partition-2
	      :value 0))
   :context context)

  (declare-reward-function
   :name 'a-box-in-paris
   :cases '(cases 
	    (:case (exists (box.box)
			   (box-in box paris))
	      :alias a-box-in-paris-partition-1
	      :value 1)
	    (:case (not (exists (box.box)
				(box-in box paris)))
	      :alias a-box-in-paris-partition-2
	      :value 0))
   :context context))

(deftest test-disjoin-cases ()
  (let ((context (make-instance 'top-level-context)))
    (define-domain-for-tests context)
    (let* ((rome-cases (reward-cases (lookup 'a-box-in-rome
					     'reward-function context)))
	   (paris-cases (reward-cases (lookup 'a-box-in-paris
					      'reward-function context)))
	   (cases (disjoin-cases rome-cases paris-cases)))
      (is (typep cases 'cases-term))
      (let ((sdp-cases (sdp-cases cases)))
	(is (= (length sdp-cases) 4))
	(is (eq (first sdp-cases) (first (sdp-cases rome-cases))))
	(is (eq (second sdp-cases) (second (sdp-cases rome-cases))))
	(is (eq (third sdp-cases) (first (sdp-cases paris-cases))))
	(is (eq (fourth sdp-cases) (second (sdp-cases paris-cases))))))))

(deftest test-sort-cases-01 ()
  (let ((context (make-instance 'top-level-context)))
    (define-domain-for-tests context)
    (let* ((rome-cases (reward-cases (lookup 'a-box-in-rome
					     'reward-function context)))
	   (paris-cases (reward-cases (lookup 'a-box-in-paris
					      'reward-function context)))
	   (cases-1 (disjoin-cases rome-cases paris-cases))
	   (cases-2 (sort-cases cases-1)))
      (is (eq cases-1 cases-2))
      (let ((sdp-cases (sdp-cases cases-2)))
	(is (eq (first sdp-cases) (first (sdp-cases rome-cases))))
	(is (eq (second sdp-cases) (first (sdp-cases paris-cases))))
	(is (= (value (third sdp-cases)) 0))
	(is (= (value (fourth sdp-cases)) 0))
	(is (not (eq (third sdp-cases) (fourth sdp-cases))))))))


(deftest test-sort-cases-02 ()
  (let ((context (make-instance 'top-level-context)))
    (define-domain-for-tests context)
    (let* ((rome-cases (reward-cases (lookup 'a-box-in-rome
					     'reward-function context)))
	   (paris-cases (reward-cases (lookup 'a-box-in-paris
					      'reward-function context)))
	   (cases-1 (disjoin-cases paris-cases rome-cases))
	   (cases-2 (sort-cases cases-1)))
      (is (eq cases-1 cases-2))
      (let ((sdp-cases (sdp-cases cases-2)))
	(is (eq (first sdp-cases) (first (sdp-cases rome-cases))))
	(is (eq (second sdp-cases) (first (sdp-cases paris-cases))))
	(is (= (value (third sdp-cases)) 0))
	(is (= (value (fourth sdp-cases)) 0))
	(is (not (eq (third sdp-cases) (fourth sdp-cases))))))))


