;;; Examples for Terms 

(asdf:defsystem #:terms-examples
  :serial t
  :description "Examples for Terms"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #:fiveam
               #:iterate
	       #-(or ecl abcl)
	       #:closer-mop
	       #:terms)
  :components ((:file "macros-for-examples")
	       (:file "support-for-examples")
	       (:file "examples")))
