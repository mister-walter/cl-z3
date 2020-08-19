;; (load "~/quicklisp/setup.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-bitvectors
  (:use :cl :z3))

(in-package :z3-bitvectors)

(solver-init)

(solver-push)
(z3-assert
 ;; A bitvector variable must be given a length.
 (v (:bv 5))
 ;; the nil below indicates that we want to treat v as an unsigned
 ;; value when we convert it to an integer.
 (= (bv2int v nil) 20))
;; Bitvectors are converted to CL bitvector objects.
(check-sat)
(solver-pop)

;; You can represent bitvector constants using int2bv
(solver-push)
(z3-assert
 (v (:bv 10) x (:bv 4))
 ;; the first number is the integer to convert to a bitvector, the
 ;; second number is the length of the bitvector to convert to.
 (and (= v (int2bv 27 10))
      (= x (int2bv -4 4))))
(check-sat)
;; note that Z3's bitvectors are sign agnostic - individual operators
;; may treat them as signed or unsigned values.
;; We always interpret bitvectors as unsigned integers.
(solver-pop)

;; You can also write bitvector constants using `bv` with lists of
;; {0,1} or booleans.
;; Note that Z3 treats bitvectors as big-endian when converting back to a number.
(solver-push)
(z3-assert
 (v (:bv 5) x (:bv 3))
 (and (= v (bv 0 0 0 1 1))
      (= x (bv t nil nil))))
(check-sat)
(solver-pop)

;; There are many functions that operate on bitvectors; see
;; src/ffi/z3-api.lisp if you'd like more information on any of them.
;; See src/z3/ast.lisp for a mapping of z3's function names to the
;; names that are usable in z3-assert.
(solver-push)
(z3-assert
 (v (:bv 4))
 (= v (bvadd (int2bv 15 4) (int2bv -1 4))))
(check-sat)
(solver-pop)
