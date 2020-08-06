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
     (:file "z3-ast-containers")
     (:file "z3-api")
     ))
