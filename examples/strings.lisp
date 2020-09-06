;; (load "~/quicklisp/setup.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-strings
  (:use :cl :z3))

(in-package :z3-strings)

(solver-init)

(solver-push)
(z3-assert
 (v :string)
 (= v "Hello, World!"))
(check-sat)
(solver-pop)

;; Strings are just a specialized sequence type - you can perform all
;; the normal sequence operations on them.
;; Note that the string type is equivalent to (:seq (:bv 8)) - i.e. 8-bit bitvectors
;; It's often easier to use seq-at instead of seq-nth. seq-at will
;; return a length-1 subsequence at the given offset, as opposed to
;; the element at that offset.
(solver-push)
(z3-assert
 ;; A sequence variable must have the element sort specified
 (v :string)
 (= (seq-length v) 20))
;; Sequences are converted to CL lists.
(check-sat)
(solver-pop)

(solver-push)
(z3-assert
 (x :string y :string)
 (= (seq-concat x y) "Hello, World!"))
(check-sat)
(solver-pop)

;; Note the use of seq-at instead of seq-nth
(solver-push)
(z3-assert
 (x :string)
 (and (> (seq-length x) 2)
      (= (seq-at x 2) "a")))
(check-sat)
(solver-pop)

;; Note that seq-at is underspecified if the index is out of bounds.
(solver-push)
(z3-assert
 (x :string y :string)
 (and
  (<= (seq-length x) 3)
  (= (seq-at x 4) y)))
(check-sat)
(solver-pop)

;; Z3 also provides lexicographic comparison operators
(solver-push)
(z3-assert
 (x :string y :string z :string)
 (and
  (seq-prefix "a" x)
  (seq-prefix "ab" y)
  (seq-prefix "a" z)
  (str-lt x y)
  (str-lt y z)))
(check-sat)
(solver-pop)

;; And you can convert between strings and integers
(solver-push)
(z3-assert
 (x :string y :int)
 (and (= x (int-to-str 5))
      (= y (str-to-int "3"))))
(check-sat)
(solver-pop)
