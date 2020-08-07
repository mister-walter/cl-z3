(in-package :z3)

(defmethod initialize-instance :after ((obj solver) &key)
  (with-slots (handle context) obj
    (z3-solver-inc-ref context handle)))

(defun make-simple-solver (&optional context)
  (let ((ctx (or context (make-instance 'context))))
    (make-instance 'solver
                   :handle (z3-mk-simple-solver ctx)
                   :context ctx)))

(defgeneric get-context (v)
  (:documentation "Get a context object from another value")
  (:method (v)
           (error "get-context unsupported for values of type ~S" (type-of v)))
  (:method ((v solver))
           (slot-value v 'context)))

(defgeneric solver-assert (solver stmt)
  (:documentation "Assert a statement in a solver")
  (:method (solver stmt)
           ;; TBD try to convert
           (error "Currently we only support stmt arguments that are ASTs."))
  (:method (solver (stmt ast))
           (z3-solver-assert (get-context solver) solver stmt)))

#|
;; This is an example of using the z3 bound functions to find a satisfying assignment
(let* ((config (Z3-mk-config))
       (c (Z3-mk-context config))
       (solver (z3-mk-simple-solver c))
       (x (z3-mk-const c
                       (z3-mk-string-symbol c "X")
                       (z3-mk-bool-sort c)))
       (y (z3-mk-const c
                       (z3-mk-string-symbol c "Y")
                       (z3-mk-bool-sort c)))
       (_ (z3-solver-assert c solver (z3-mk-xor c x y))))
  (list (z3-solver-check c solver)
        (z3-solver-get-model c solver)))
|#

(defparameter *default-context* (make-instance 'context))
(defparameter *default-solver* (make-simple-solver *default-context*))


;; TODO: integrate defdata's types where possible
;; e.g. automatically convert defdatas into "equivalent" Z3 sorts
;; but a fair amount of work is needed here to convert back and forth between z3 values and defdata values.
(defmacro make-var-decls (decls context)
  (cons 'list
        (loop for (var ty) on decls by #'cddr
              collect `(cons ',var (get-sort ,ty ,context)))))

;;(make-var-decls (x :int y :bool) ctx)

(defun solver-init ()
  (setf *default-context* (make-instance 'context))
  (setf *default-solver* (make-simple-solver *default-context*)))

(defmacro z3-assert (var-decls stmt &optional context solver)
  (when (oddp (length var-decls)) (error "Each declared variable must have a type."))
  `(solver-assert (or ,solver *default-solver*)
                  (convert-to-ast (or ,context *default-context*)
                                  ',stmt
                                  (make-var-decls ,var-decls (or ,context *default-context*)))))

(defun get-model (&optional context solver)
  "Get the model object for the last solver-check[-assumptions] call.
   Will invoke the error handler if no model is available."
  (let* ((ctx (or context *default-context*))
         (slv (or solver *default-solver*)))
    (make-instance 'model
                   :handle (z3-solver-get-model ctx slv)
                   :context ctx)))

(defun check-sat (&optional context solver)
  (let* ((ctx (or context *default-context*))
         (slv (or solver *default-solver*)))
    (match (z3-solver-check ctx slv)
           (:L_TRUE (model-constants-to-assignment (get-model context solver) context)) ;; assertions are satisfiable (a model may be generated)
           (:L_FALSE :UNSAT) ;; assertions are not satisfiable (a proof may be generated)
           ;; TODO: in the undef case we may want to get the model and see if the assignment satisfies the assertions
           ;; if so we can return it.
           (:L_UNDEF :UNKNOWN)))) ;; get_model may succeed but the model may not satisfy the assertions


;; i.e. will use default context and solver
;; maybe will init for you, optional context and solver
;; use defunc/definec code to figure out types
;; and then export types to z3 sorts
#|

(solver-init)
(z3-assert
  (x :bool y :bool)
  (and x y t))

(CONVERT-TO-AST (OR NIL *DEFAULT-CONTEXT*) '(AND X Y T)
                '((X . 94544999634200) (Y . 94544999634200)))

                               (MAKE-VAR-DECLS (X :BOOL Y :BOOL)
                                               (OR NIL *DEFAULT-CONTEXT*))))

(z3-solver-check *default-context* *default-solver*)

(make-instance 'model
:handle
(z3-solver-get-model *default-context* *default-solver*)
:context *default-context*)

(defvar blah (make-instance 'model
:handle
(z3-solver-get-model *default-context* *default-solver*)
:context *default-context*))

(defvar bleh (make-instance 'model
:handle
(z3-solver-get-model *default-context* *default-solver*)
:context *default-context*))

|#

#|
(check-sat)
|#
