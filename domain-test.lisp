;;; -*- Mode: Lisp; common-lisp-style: poem -*-

;;; Copyright (c) 2012 Matthias Hölzl
;;; Copyright (c) 2012 Lenz Belzner
;;;
;;; This file is licensed under the MIT license; see the file LICENSE
;;; in the root directory for further information.

(in-package #:action-theory)

(declare-sort :name 'box)
(declare-sort :name 'city)
(declare-sort :name 'truck)

#+(or)
(declare-constant :name 'rome :sort 'city)
#+(or)
(declare-constant :name 'paris :sort 'city)

(declare-natures-choice
 :prototype '(load-succeed box.box truck.truck)
 :probabilities '(cases (:case true :value 0.99)))

(declare-natures-choice
 :prototype '(load-fail box.box truck.truck)
 :probabilities '(cases (:case true :value 0.01)))

(declare-primitive-action
 :prototype '(load box.box truck.truck)
 :precondition '(exists (city.city)
		 (and (box-in box city)
		      (truck-in truck city)))
 :natures-choices '(load-succeed load-fail))

(declare-fluent
 :prototype '(truck-in truck.truck city.city)
 :sort 'boolean
 :successor-state-axiom '(or (and (truck-in truck city)
			      (forall (dest.city)
			       (not (and (not (= city dest))
				     (= action (drive-succeed truck dest))))))
			  (= action (drive-succeed truck city))))

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
	    :value 0)))

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
	    :value 0)))

(declare-reward-function
 :name 'a-strange-reward
 :cases '(cases 
	  (:case (box-in ?box ?somewhere)
	    :alias a-box-somewhere
	    :value 1)
	  (:case (not (box-in ?box paris))
	    :alias a-box-in-paris-partition-2
	    :value 0))
 :context (make-instance 'local-context
            :enclosing-context *default-context*))

(defparameter *rome-cases*
  (reward-cases (lookup 'a-box-in-rome 'reward-function *default-context*)))
(defparameter *paris-cases*
  (reward-cases (lookup 'a-box-in-paris 'reward-function *default-context*)))
(defparameter *strange-cases*
  (reward-cases (lookup 'a-strange-reward 'reward-function *default-context*)))

(defparameter *disjoint-cases*
  (disjoin-cases *rome-cases* *paris-cases*))
(defparameter *added-cases*
  (add-cases *rome-cases* *paris-cases*))
(defparameter *multiplied-cases* 
  (multiply-cases *rome-cases* *paris-cases*))
(defparameter *existentially-quantified-cases*
  (existentially-quantify-cases *disjoint-cases*))
(defparameter *existentially-quantified-cases-1*
  (existentially-quantify-cases *strange-cases*))
(defparameter *partitioned-cases*
  (partition-cases *multiplied-cases*))
(defparameter *casemaxed-cases*
  (casemax *multiplied-cases*))
