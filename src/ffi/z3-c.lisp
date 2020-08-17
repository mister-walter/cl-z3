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

; Better to use environment variables; see z3-grovel.lisp
; But on MacOs X this requires turning off some security settings (SIP)

; (pushnew #P"/Users/pete/bin/lib/" *foreign-library-directories* :test #'equal)

(use-foreign-library libz3)
