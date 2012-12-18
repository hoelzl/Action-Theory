;;; Testing
;;; =======


#+5am
(5am:def-suite action-theory-suite
  :description "The suite containing all tests for Action-Theory.")

#+5am
(5am:def-suite action-theory-utilities-suite
  :in action-theory-suite
  :description "Tests for utilities.")

#+5am
(5am:def-suite action-theory-macro-suite
  :in action-theory-suite
  :description "Tests for macros.")

#+5am
(5am:def-suite action-theory-syntax-suite
  :in action-theory-suite
  :description "Tests for the syntax representation.")

#+5am
(5am:def-suite action-theory-situation-suite
  :in action-theory-suite
  :description "Tests for the situations.")

#+5am
(5am:def-suite action-theory-parser-suite
  :in action-theory-suite
  :description "Tests for the parser.")
