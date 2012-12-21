;;; action-theory-test

(asdf:defsystem #:action-theory-test
  :serial t
  :description "Tests for the action theory framework"
  :version "0.0.1"
  :author "Matthias Hoelzl <tc@xantira.com>"
  :license "MIT, see file LICENSE"
  :depends-on (#:alexandria
	       #+swank
               #:hu.dwim.stefil+swank
               #-swank
               #:hu.dwim.stefil
               #:iterate
	       #-(or ecl abcl) #:closer-mop
	       #:action-theory)
  :components ((:file "test-package")
	       (:file "test-suites")
	       (:file "test-utilities")
	       (:file "test-cases")
	       (:file "test-terms")
	       (:file "test-situation")
	       (:file "test-parser")))
