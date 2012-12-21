;;;; action-theory.asd

(asdf:defsystem #:action-theory
  :serial t
  :description "A general representation for action theories"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :author "Lenz Belzner <belzner@pst.ifi.lmu.de>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #-(or ecl abcl) #:closer-mop
               #:iterate
	       #:snark)
  :components ((:file "package-exports")
	       (:file "packages")
	       (:file "utilities")
	       (:file "macros")
	       (:file "context")
	       (:file "prototypes")
	       (:file "sorts")
	       (:file "primitive-actions")
	       (:file "natures-choices")
	       (:file "fluents")
	       (:file "terms")
	       (:file "cases")
	       (:file "compilation-unit")
	       (:file "situation")
	       (:file "term-operations")
	       (:file "parser")))
