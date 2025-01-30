;; SPDX-FileCopyrightText: Copyright (c) 2020 Andrew T. Walter <me@atwalter.com>
;; SPDX-License-Identifier: MIT
(load "try-load-quicklisp.lisp")
(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-sets
  (:use :cl :z3))

(in-package :z3-sets)

(solver-init)

(solver-push)

;; unfortunately the syntax for creating sets is currently pretty
;; verbose - you need to create an empty set and set-add, or create a
;; full set and set-del.
(z3-assert (x :bool y (:set :int) z :bool w (:set :int))
           (and (= x (set-member 1 (set-add (empty-set :int) 1)))
                (= y (set-difference (set-add (set-add (empty-set :int) 1) 2)
                                     (set-add (empty-set :int) 1)))
                ;; complementing the full set \ 2 produces the same set as y
                (= z (= y (set-complement (set-del (full-set :int) 2))))
                (= w (full-set :int))))

;; we represent sets as alists when converting to Lisp. Every set must
;; have a :default key in the alist, typically associated with nil.
(check-sat)
(get-model)
(get-model-as-assignment)
;; Note that we also get an |array-ext| element in our output here.
;; This is an artifact of how Z3's theory of arrays works.
;; See the following link for more information:
;; https://github.com/Z3Prover/z3/discussions/6089

