(defpackage #:z3
  (:documentation "The Lisp-Z3 interface frontend.")
  (:use #:cl #:z3-c)
  (:shadow #:sort #:optimize)
  (:import-from :trivia :match)
  (:import-from :flexi-streams :octets-to-string)
  (:import-from :cffi :translate-to-foreign :foreign-string-to-lisp :with-foreign-pointer :mem-ref)
  (:export #:solver-init #:set-solver
           #:make-simple-solver #:make-composite-solver #:make-solver-from-tactic
           #:make-optimizer
           #:make-tactic
           #:solver-push #:solver-pop #:print-solver #:solver-reset
           #:z3-assert #:z3-assert-fn
           #:z3-assert-soft #:optimize-minimize #:optimize-maximize
           #:check-sat #:get-model #:get-model-as-assignment
           #:convert-to-ast #:ast-to-value
           #:register-enum-sort #:register-tuple-sort
           #:z3-object-to-string
           #:set-global-param #:get-global-param
           #:set-params
           #:*default-context* #:*default-solver*
           #:parse-smt2-file #:parse-smt2-string
           #:ast-vector-to-list
           #:*ALGEBRAIC-NUMBER-PRINT-MODE*
           #:*ALGEBRAIC-NUMBER-PRINT-DECIMAL-PRECISION*
           #:*ALGEBRAIC-NUMBER-CONVERT-MODE*
           #:*ALGEBRAIC-NUMBER-CONVERT-DECIMAL-PRECISION*))
