(in-package :z3)

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

(model-constants-to-assignment blah)
|#

(defun model-constants-to-assignment (model &optional context)
  (let* ((ctx (or context *default-context*))
         (const-decls
          (loop for i below (z3-model-get-num-consts ctx model)
                collect (z3-model-get-const-decl ctx model i))))
    (loop for decl in const-decls
          for name = (z3-get-symbol-string ctx (z3-get-decl-name ctx decl))
          for value = (z3-model-get-const-interp ctx model decl)
          when (not (equal value 0)) ;; may return null if the model doesn't assign an interpretation for the func-decl
          collect (list (intern name) (ast-to-value value ctx)))))

