(in-package :z3)

(defun make-simple-solver (&optional context)
  (let ((ctx (or context (make-instance 'context))))
    (make-instance 'solver
                   :handle (z3-mk-simple-solver ctx)
                   :context ctx)))

(defgeneric get-context (v)
  (:documentation "Get a context object from another value")
  (:method (v)
           (error "get-context unsupported for values of type ~S" (type-of v)))
  (:method ((v solver))
           (slot-value v 'context)))

(defgeneric solver-assert (solver stmt)
  (:documentation "Assert a statement in a solver")
  (:method (solver stmt)
           ;; TBD try to convert
           (error "Currently we only support stmt arguments that are ASTs."))
  (:method (solver (stmt ast))
           (z3-solver-assert (get-context solver) solver stmt)))

#|
;; This is an example of using the z3 bound functions to find a satisfying assignment
(let* ((config (Z3-mk-config))
       (c (Z3-mk-context config))
       (solver (z3-mk-simple-solver c))
       (x (z3-mk-const c
                       (z3-mk-string-symbol c "X")
                       (z3-mk-bool-sort c)))
       (y (z3-mk-const c
                       (z3-mk-string-symbol c "Y")
                       (z3-mk-bool-sort c)))
       (_ (z3-solver-assert c solver (z3-mk-xor c x y))))
  (list (z3-solver-check c solver)
        (z3-solver-get-model c solver)))
|#
