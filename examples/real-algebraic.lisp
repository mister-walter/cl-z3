(load "~/quicklisp/setup.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-real-algebraic
  (:use :cl :z3))

(in-package :z3-real-algebraic)

(solver-init)

(solver-push)
(z3-assert
 (x :real)
 (and (not (= x 0))
      (= (* 20 x) 1)))
(check-sat)
(solver-pop)

(solver-push)
(z3-assert
 (x :real)
 (= (* x x) 2))
(check-sat)
(solver-pop)
