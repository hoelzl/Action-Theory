(in-package #:action-theory)

(declare-sort :name 'box)
(declare-sort :name 'city)
(declare-sort :name 'truck)

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
