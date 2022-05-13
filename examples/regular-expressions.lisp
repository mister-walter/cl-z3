(load "try-load-quicklisp.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-regex
  (:use :cl :z3))

(in-package :z3-regex)

(solver-init)

(solver-push)
;; Note that re-empty takes in a regex sort, NOT the sequence sort that the regex is over.
(z3-assert
 (x :string)
 (seq-in-re x (re-empty (:regex :string))))
(check-sat)
(solver-pop)

(solver-push)
(z3-assert
 (x :string)
 (seq-in-re x (re-full (:regex :string))))
(check-sat)
(solver-pop)

(solver-push)
(z3-assert
 (x :string)
 (and (> (str.len x) 3)
      (seq.in.re x (re.+ (re.union (re.range "a" "b") (re.range "d" "e"))))))
(check-sat)
(solver-pop)

;; Regular expressions *must* be constant expressions.
;; The following 2 examples crash due to this.
#|
(solver-push)
(z3-assert
 (x (:regex :string))
 (and (seq-in-re "Hello, World!" x)
      (seq-in-re "Goodbye, World!" x)))
(check-sat)
(solver-pop)
|#

#|
(solver-push)
(z3-assert
 (x :string y :string)
 (seq-in-re y (re-loop (seq-to-re x) 2 4)))
(check-sat)
(solver-pop)
|#
