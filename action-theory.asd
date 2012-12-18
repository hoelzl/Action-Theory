;;;; action-theory.asd

(asdf:defsystem #:action-theory
  :serial t
  :description "A general representation for action theories"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #-(or ecl abcl) #:closer-mop
	       #:fiveam
               #:iterate
	       #:snark)
  :components ((:file "package-exports")
	       (:file "packages")
	       (:file "utilities")
	       (:file "macros")
	       (:file "compilation-context")
	       (:file "primitive-actions")
	       (:file "fluents")
	       (:file "terms")
	       (:file "compilation-unit")
	       (:file "situation")
	       (:file "term-operations")
	       (:file "parser")))
