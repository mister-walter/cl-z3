(defpackage #:z3
  (:use #:cl #:z3-c)
  (:shadow #:sort #:optimize)
  (:import-from :trivia :match)
  (:import-from :cffi :translate-to-foreign)
  (:export #:solver-init #:solver-push #:solver-pop #:z3-assert #:check-sat #:*default-context* #:*default-solver*))
