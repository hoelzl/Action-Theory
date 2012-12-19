(in-package #:action-theory)

(declare-sort :name 'box)
(declare-sort :name 'city)
(declare-sort :name 'truck)

(declare-primitive-action
 :prototype '(load box.box truck.truck)
 :precondition '(exists (city.city)
		 (and (box-in box city)
		      (truck-in truck city))))
