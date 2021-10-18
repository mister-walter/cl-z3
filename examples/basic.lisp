(load "~/quicklisp/setup.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-basic
  (:use :cl :z3))

(in-package :z3-basic)

(solver-init)
;; Create a new scope and enter it. We can (solver-pop) later to exit this scope.
;; We'll see how scopes can be used later.
(solver-push)

(z3-assert (x :bool y :int)
           (and x (>= y 5)))
;; This returns a list of assignments suitable for use in a let binding if the assertion(s) are satisfiable
;; Otherwise it returns :unknown or :unsat
(check-sat)

;; As an example, let's check that the assertion expression really evaluates to true under the model Z3 found.
(eval `(let ,(check-sat) (and x (>= y 5))))

;;;; SCOPES ;;;;
;; Say we want to see what happens if we add the assertion (not x).
;; We can create a new scope and enter it. This scope contains all of the assertions and assumptions that were introduced in the current scope.
;; Z3 may also be able to reuse some work that it already did when we called (check-sat) in the above scope.
(solver-push)
(z3-assert (x :bool)
           (not x))
;; This returns :UNSAT because no model can make both (and x (>= y 5)) and (not x) true.
(check-sat)
;; This will exit the scope that we just created. At this point, the only assertion that remains is (and x (>= y 5)).
(solver-pop)
;; To confirm, note that this is the same assignment that Z3 gave when we called (check-sat) after our first (z3-assert ...) call.
(check-sat)
;; Now we'll return to the top level scope, which currently contains no assertions or assumptions.
(solver-pop)

;; In some situations, Z3 may note that a variable doesn't matter
(solver-push)
(z3-assert (x :bool y :int)
           (and (or x (not x)) (>= y 5)))
;; Note that X doesn't appear in the assignment returned by check-sat!
(check-sat)
;; We may change this behavior in the future so that it's possible to evaluate the above assertion under the result of (check-sat).
(solver-pop)
