(defpackage #:z3
  (:use #:cl #:z3-c)
  (:shadow #:sort #:optimize)
  (:import-from :trivia :match)
  (:import-from :cffi :translate-to-foreign)
  (:export #:solver-init #:solver-push #:solver-pop
           #:z3-assert #:check-sat
           #:convert-to-ast #:ast-to-value
           #:register-finite-domain-sort #:register-enum-sort #:register-tuple-sort
           #:*default-context* #:*default-solver*))
