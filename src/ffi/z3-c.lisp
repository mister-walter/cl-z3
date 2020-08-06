#|
(pushnew (truename "/home/drew/lisp-z3/") ql:*local-project-directories* )
(ql:register-local-projects)
(ql:quickload :lisp-z3/ffi)
|#

(in-package :z3-c)

(define-foreign-library libz3
  (:darwin (:or "libz3.4.8.dylib" "libz3.dylib"))
  (:unix (:or "libz3.so.4.8" "libz3.so"))
  (t (:default "libz3")))

(use-foreign-library libz3)
