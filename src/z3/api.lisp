(in-package :z3)

(defparameter *default-context* (make-instance 'context))
(defparameter *default-solver* (make-simple-solver *default-context*))

;; TODO: integrate defdata's types where possible
;; e.g. automatically convert defdatas into "equivalent" Z3 sorts
;; but a fair amount of work is needed here to convert back and forth between z3 values and defdata values.
(defmacro make-var-decls (decls context)
  (cons 'list
        (loop for (var ty) on decls by #'cddr
              collect `(cons ',var (get-sort ,ty ,context)))))

(defun make-var-decls-fn (decls context)
  (loop for (var ty) on decls by #'cddr
        collect (cons var (get-sort ty context))))

;;(make-var-decls (x :int y :bool) ctx)

(defun solver-init ()
  (setf *default-context* (make-instance 'context))
  (setf *default-solver* (make-simple-solver *default-context*)))

(defun solver-push (&optional solver)
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (z3-solver-push ctx slv)))

(defun solver-pop (&optional solver &key (n 1))
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (z3-solver-pop ctx slv n)))

(defun solver-reset (&optional solver)
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (z3-solver-reset ctx slv)))

(defun z3-assert-fn (var-decls stmt &optional solver)
  (when (oddp (length var-decls)) (error "Each declared variable must have a type."))
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (solver-assert slv
                   (convert-to-ast stmt
                                   (make-var-decls-fn var-decls ctx)
                                   ctx))))

(defmacro z3-assert (var-decls stmt &optional solver)
  (when (oddp (length var-decls)) (error "Each declared variable must have a type."))
  `(let* ((slv (or ,solver *default-solver*))
          (ctx (get-context slv)))
     (solver-assert slv
                    (convert-to-ast ',stmt
                                    (make-var-decls ,var-decls ctx)
                                    ctx))))

(defun get-model (&optional solver)
  "Get the model object for the last solver-check[-assumptions] call.
   Will invoke the error handler if no model is available."
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (make-instance 'model
                   :handle (z3-solver-get-model ctx slv)
                   :context ctx)))

(defun check-sat (&optional solver)
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (match (z3-solver-check ctx slv)
           (:L_TRUE (model-constants-to-assignment (get-model solver) ctx)) ;; assertions are satisfiable (a model may be generated)
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

(describe blah)

(defvar bleh (make-instance 'model
:handle
(z3-solver-get-model *default-context* *default-solver*)
:context *default-context*))

(describe bleh)

|#

#|
(check-sat)
|#
