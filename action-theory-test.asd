;;; terms-integration-tests

(asdf:defsystem #:terms-tests
  :serial t
  :description "Integration tests for Terms"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #:fiveam
               #:iterate
	       #-(or ecl abcl)
	       #:closer-mop
	       #:terms)
  :components ((:file "test-suites")
	       (:file "test-utilities")
	       (:file "test-terms")
	       (:file "test-situation")
	       (:file "test-parser")
	       (:file "test-interpreter")))
