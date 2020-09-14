(defsystem "lisp-z3/ffi"
    :defsystem-depends-on ("cffi-grovel")
    :depends-on ("cffi")
    :serial t
    :pathname "src/ffi"
    :components
    ((:file "package")
     (:cffi-grovel-file "z3-grovel")
     (:file "z3-c-types")
     (:file "z3-c")
     (:file "z3-api")
     ))

(defsystem "lisp-z3/z3"
  :depends-on ("lisp-z3/ffi" "trivia")
  :serial t
  :pathname "src/z3"
  :components
  ((:file "package")
   (:file "match-extensions")
   (:file "util")
   (:file "types")
   (:file "ast-vector")
   (:file "sorts")
   (:file "ast")
   (:file "model")
   (:file "solver")
   (:file "api")
   ))

(defsystem "lisp-z3"
  :depends-on ("lisp-z3/z3" "trivia")
  :serial t
  :components
  ())
