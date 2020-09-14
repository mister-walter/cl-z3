(in-package :z3)

(defgeneric ast-vector-to-list (vec)
  (:documentation "Convert an AST vector to a Lisp list")
  (:method ((vec ast-vector))
           (loop for i below (z3-ast-vector-size (get-context vec) vec)
                 collect (make-instance 'ast
                                        :handle (z3-ast-vector-get (get-context vec) vec i)
                                        :context (get-context vec)))))

(defun list-to-ast-vector (list ctx)
  (let ((vec (z3-mk-ast-vector ctx)))
    (loop for elt in list
          do (z3-ast-vector-push ctx vec elt))
    vec))
