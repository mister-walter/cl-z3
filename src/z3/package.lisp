(defpackage #:z3
  (:use #:cl #:z3-c)
  (:shadow #:sort #:optimize)
  (:import-from :trivia :match)
  (:import-from :flexi-streams :octets-to-string)
  (:import-from :cffi :translate-to-foreign)
  (:export #:solver-init #:set-solver
           #:make-simple-solver #:make-composite-solver #:make-solver-from-tactic
           #:make-tactic
           #:solver-push #:solver-pop #:print-solver #:solver-reset
           #:z3-assert #:z3-assert-fn #:check-sat
           #:convert-to-ast #:ast-to-value
           #:register-finite-domain-sort #:register-enum-sort #:register-tuple-sort
           #:z3-object-to-string
           #:set-global-param #:get-global-param
           #:set-params
           #:*default-context* #:*default-solver*
           #:parse-smt2-file #:parse-smt2-string
           #:ast-vector-to-list))
