(in-package :z3)

(defun make-simple-solver (&optional context)
  (let ((ctx (or context (make-instance 'context))))
    (make-instance 'solver
                   :handle (z3-mk-simple-solver ctx)
                   :context ctx)))

(defun make-composite-solver (&optional context)
  (let ((ctx (or context (make-instance 'context))))
    (make-instance 'solver
                   :handle (z3-mk-solver ctx)
                   :context ctx)))

(defun make-solver-from-tactic (tactic &optional context)
  (let ((ctx (or context *default-context*)))
    (make-instance 'solver
                   :handle (z3-mk-solver-from-tactic ctx tactic)
                   :context ctx)))

(defgeneric solver-assert (solver stmt)
  (:documentation "Assert a statement in a solver")
  (:method (solver stmt)
           ;; TBD try to convert
           (error "Currently we only support stmt arguments that are ASTs."))
  (:method (solver (stmt ast))
           (z3-solver-assert (get-context solver) solver stmt)))

(defun get-solver-help (&optional solver)
  (let ((slv (or solver *default-solver*)))
    (z3-solver-get-help (get-context slv) slv)))

(defun get-solver-param-descrs (&optional solver)
  (let ((slv (or solver *default-solver*)))
    (make-instance 'param-descrs
                   :handle (z3-solver-get-param-descrs (get-context slv) slv)
                   :context (get-context slv))))

(defun get-solver-stats (&optional solver)
  (let ((slv (or solver *default-solver*)))
    (make-instance 'statistics
                   :handle (z3-solver-get-statistics (get-context slv) slv)
                   :context (get-context slv))))

(defmacro set-params (settings &optional solver)
  `(let ((slv (or ,solver *default-solver*)))
     (z3-solver-set-params (get-context slv) slv (make-params ,settings))))

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
