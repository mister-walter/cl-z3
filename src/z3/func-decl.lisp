(in-package :z3)

(defun func-decl-domain (decl context)
  "Get the domain of the function declaration, as a list of sorts."
  (loop for i below (z3-get-domain-size context decl)
        collect (make-instance 'sort
                               :handle (z3-get-domain context decl i)
                               :context context)))

(defun func-decl-range (decl context)
  "Get the range of the function declaration, as a sort."
  (make-instance 'sort
                 :handle (z3-get-range context decl)
                 :context context))
