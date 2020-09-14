(defpackage #:z3
  (:use #:cl #:z3-c)
  (:shadow #:sort #:optimize)
  (:import-from :trivia :match)
  (:import-from :cffi :translate-to-foreign)
  (:export #:solver-init #:solver-push #:solver-pop
           #:z3-assert #:check-sat
           #:convert-to-ast #:ast-to-value
           #:register-finite-domain-sort #:register-enum-sort #:register-tuple-sort
           #:z3-object-to-string
           #:*default-context* #:*default-solver*
           #:parse-smt2-file
           #:ast-vector-to-list))
