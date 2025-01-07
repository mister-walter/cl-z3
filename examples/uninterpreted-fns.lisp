(load "try-load-quicklisp.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-uninterp-fns
  (:use :cl :z3))

(in-package :z3-uninterp-fns)

(solver-init)

(solver-push)
;; To use an uninterpreted function in a z3-assert, you must declare
;; its signature in the same place that you declare variables.
;; (:fn (:int) ;int) represents a function that takes in a single
;; parameter that has :int sort, and produces a value of :int sort.
(z3-assert (f (:fn (:int) :int))
           ;; Calling an unintepreted function is done using the _
           ;; operator, followed by the function name and any arguments.
           (and (= (_ f 0) 3)
                (= (_ f 1) 8)))
(check-sat)
(get-model)
;; The model produced will contain an entry for the uninterpreted
;; function f. This entry consists of a list (:fn <type> <alist>),
;; where <type> is a list where the first element contains the sorts
;; of the parameters of f, and the second element is the sort of the
;; result of f, and <alist> is an alist representing a map from
;; parameter values to the function's value. Additionally, there will
;; be an entry in the alist whose car is :default, representing the
;; default value of the function when no other entry matches the
;; provided arguments.
(get-model-as-assignment)
(solver-pop)

(solver-push)
;; You can use variables and uninterpreted functions together.
(z3-assert (x :int f (:fn (:int) :int))
           (= (_ f x) (+ x 1)))
(check-sat)
(get-model)
(solver-pop)

(solver-push)
;; You can use multiple uninterpreted functions together and use their
;; results in calls to each other.
(z3-assert (f (:fn (:int) :string)
            g (:fn (:string) (:bv 4))
            h (:fn ((:bv 4)) :int))
           (and (= (_ h (_ g (_ f 3))) 5)
                (= (_ h (_ g (_ f 1))) 20)
                (= (_ f 1) "hello")
                (= (_ f 2) "world!")))
(check-sat)
(get-model)
(solver-pop)

(solver-push)
;; Uninterpreted functions can take multiple arguments.
(z3-assert (f (:fn (:int :int) :string))
           (and (= (_ f 0 0) "h")
                (= (_ f 0 1) "i")
                (= (_ f 1 0) "b")
                (= (_ f 1 1) "y")
                (= (_ f 1 2) "e")))
(check-sat)
(get-model)
(solver-pop)

(solver-push)
(z3-assert (f (:fn (:int) :int)
            x :int)
           (and (= (_ f x) x)
                (not (= (_ f (_ f x)) x))))
(check-sat)
(solver-pop)

;; Note that the interface treats uninterpreted functions and
;; variables as though they are "in different namespaces", similar to
;; Common Lisp. Thus you can have a variable and an uninterpreted
;; function with the same name in an assertion. This will result in a
;; model with two bindings for that name, one for the function and one
;; for the variable.
(solver-push)
(z3-assert (f :int) (= f 0))
(z3-assert (f (:fn (:int) :int)) (= (_ f 0) 4))
;; just to show you can do this in a single assertion
(z3-assert (g :int g (:fn (:int) :int))
           (= (_ g g) g))
(check-sat)
(get-model)
(solver-push)
