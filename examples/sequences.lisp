(load "try-load-quicklisp.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-sequences
  (:use :cl :z3))

(in-package :z3-sequences)

(solver-init)

(solver-push)
(z3-assert
 ;; A sequence variable must have the element sort specified
 (v (:seq :int))
 (= (seq-length v) 20))
;; Sequences are converted to CL lists.
(check-sat)
(solver-pop)

;; You can construct a sequence from constants using the `seq` operator
(solver-push)
(z3-assert
 (x (:seq :int) y (:seq :int))
 (= (seq-concat x y) (seq 1 2 3 4 5)))
(check-sat)
(solver-pop)

;; You can create a unit sequence with seq-unit and any value
(solver-push)
(z3-assert
 (x (:seq :int) y :int)
 (= x (seq-unit (+ 1 y))))
(check-sat)
(solver-pop)

;; You can create an empty sequence with seq-empty and an element sort
(solver-push)
(z3-assert
 (x (:seq :int))
 (= (seq-concat (seq-empty :int) x) (seq-unit 1)))
(check-sat)
(solver-pop)

;; You can access the ith element of a sequence with seq-nth.
(solver-push)
(z3-assert
 (x (:seq :int))
 (and
  (>= (seq-length x) 3)
  (= (seq-nth x 2) (+ (seq-nth x 0) (seq-nth x 1)))))
(check-sat)
(solver-pop)

;; Note that seq-nth is underspecified if the index is out of bounds.
(solver-push)
(z3-assert
 (x (:seq :int) y :int)
 (and
  (<= (seq-length x) 3)
  (= (seq-nth x 4) y)))
(check-sat)
(solver-pop)

;; You can have sequences of more complicated types too
(solver-push)
(z3-assert
 (x (:seq (:bv 5)))
 (= (seq-length x) 3))
(check-sat)
(solver-pop)

#| ;; TODO currently runs for a while, possible infinite loop?
;; You can even have sequences of sequences.
(solver-push)
(z3-assert
 (x (:seq (:seq :int)))
 (= (seq-length x) 1))
(check-sat)
(solver-pop)
|#
