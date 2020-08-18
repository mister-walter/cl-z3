;; (load "~/quicklisp/setup.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-bitvectors
  (:use :cl :z3))

(in-package :z3-bitvectors)

(solver-init)

;; Here's an example of a case where we have a variable that is of a
;; tuple sort, and we have some constraints on the values of its fields.
(solver-push)
(z3-assert
 (v (:bv 5))
 ;; the nil below indicates that we want to treat v as an unsigned value when we convert it to an integer.
 (= (bv2int v nil) 20))
;; Bitvectors are converted to CL bitvector objects.
(check-sat)
(solver-pop)

;; You can write bitvector constants using lists of {0,1} or booleans
(solver-push)
(z3-assert
 (v (:bv 5))
 (= v (bvadd (bv 1 1 1 1 1) (bv nil nil nil t t))))
;; Note the overflow behavior - bvadd does two's complement signed addition.
(check-sat)
(solver-pop)
