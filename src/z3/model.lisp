(in-package :z3)

(defmethod initialize-instance :after ((obj model) &key)
  (with-slots (handle context) obj
    (z3-model-inc-ref context handle)))

#|
(import 'z3-c::(z3-model-eval z3-model-has-interp z3-model-get-func-interp
                              z3-model-get-num-consts z3-model-get-const-decl
                              z3-model-get-num-funcs z3-model-get-func-decl
                              z3-model-get-num-sorts z3-model-get-sort z3-model-get-sort-universe
                              z3-is-as-array z3-get-as-array-func-decl
                              z3-add-const-interp
                              z3-add-func-interp z3-func-interp-inc-ref z3-func-interp-dec-ref
                              z3-func-interp-get-num-entries z3-func-interp-get-entry z3-func-interp-get-else
                              z3-func-interp-set-else z3-func-interp-get-arity z3-func-interp-add-entry
                              z3-func-entry-inc-ref z3-func-entry-dec-ref z3-func-interp-get-value z3-func-entry-get-num-args z3-func-entry-get-arg))

(import 'z3-c::(z3-get-symbol-string z3-get-decl-name z3-get-sort-kind sort_kind))
|#
;;

#|

(pushnew (truename "/home/drew/lisp-z3/") ql:*local-project-directories* )
(ql:register-local-projects)
(ql:quickload :lisp-z3)
(in-package :z3)
(z3-init)
(z3-assert
  (x :bool y :bool)
  (and x y t))

(z3-assert
  (z :int)
  (= z 0))

(z3-solver-check *default-context* *default-solver*)

(defvar blah (make-instance 'model
:handle
(z3-solver-get-model *default-context* *default-solver*)
:context *default-context*))

(describe blah)

(model-constant-decls blah)
|#


#|
(defun model-constant-decls (model &optional context)
  (let* ((ctx (or context *default-context*)))
    (loop for i below (z3-model-get-num-consts ctx model)
          do (print i)
          collect (z3-model-get-const-decl ctx model i))))
 |#
(defun model-constants-to-assignment (model &optional context)
  (let* ((ctx (or context *default-context*))
         (const-decls
          (loop for i below (z3-model-get-num-consts ctx model)
                collect (z3-model-get-const-decl ctx model i))))
    (loop for decl in const-decls
          for name = (z3-get-symbol-string ctx (z3-get-decl-name ctx decl))
          for value = (z3-model-get-const-interp ctx model decl)
          collect (list (intern name) (ast-to-value value ctx)))))

