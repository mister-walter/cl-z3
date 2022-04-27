(in-package #:lisp-z3/tests)

(defmacro is-sat-assignment (expr sat-res)
  `(let ((res ,sat-res)
         (expr ',expr))
     (true (not (or (equal res :unknown) (equal res :unsat))))
     (true (eval `(let ,res ,expr)))))

(defmacro with-reset-z3 (&body body)
  `(unwind-protect
        (progn ,@body)
     (solver-reset)))

(define-test basic-assert
    (solver-init)
  (with-reset-z3
      (z3-assert (x :bool y :int)
                 (and x (>= y 5)))
    (is-sat-assignment (and x (>= y 5)) (check-sat)))
  (with-reset-z3
      (z3-assert (x :bool)
                 (and x (not x)))
    (is eq (check-sat) :unsat)))


