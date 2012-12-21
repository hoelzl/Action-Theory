(in-package #:action-theory)

(declare-sort :name 'box)
(declare-sort :name 'city)
(declare-sort :name 'truck)

(declare-primitive-action
 :prototype '(load box.box truck.truck)
 :precondition '(exists (city.city)
		 (and (box-in box city)
		      (truck-in truck city))))

(declare-fluent
 :prototype '(truck-in truck.truck city.city)
 :sort 'boolean
 :successor-state-axiom '(or (and (truck-in truck city)
			      (forall (dest.city)
			       (not (and (not (= city dest))
				     (= action (drive-succeed truck dest))))))
			  (= action (drive-succeed truck city))))
