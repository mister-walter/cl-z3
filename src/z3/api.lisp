(in-package :z3)

(defparameter *default-context* (make-instance 'context))
(defparameter *default-solver* (make-simple-solver *default-context*))

;; TODO: integrate defdata's types where possible
;; e.g. automatically convert defdatas into "equivalent" Z3 sorts
;; but a fair amount of work is needed here to convert back and forth between z3 values and defdata values.

(defun make-var-decls (decls context)
  (loop for (var ty) on decls by #'cddr
        collect (cons var (get-sort ty context))))

;;(make-var-decls '(x :int y :bool) *default-context*)

(cffi:defcallback error-handler :void ((ctx z3-c-types:context) (error-code z3-c-types:error_code))
                  (restart-case
                   (match error-code
                          (:OK (error "Z3: error handler called with error code OK - should not occur."))
                          (:SORT_ERROR (error "Z3: tried to build an AST that is not well-sorted"))
                          (:IOB (error "Z3: index out of bounds"))
                          (:INVALID_ARG (error "Z3: Invalid argument was provided"))
                          (:NO_PARSER (error "Z3: parser output is not available"))
                          (:INVALID_PATTERN (error "Z3: invalid pattern used to build a quantifier"))
                          (:MEMOUT_FAIL (error "Z3: unable to allocate memory"))
                          (:FILE_ACCESS_ERROR (error "Z3: unable to access file"))
                          (:INTERNAL_FATAL (error "Z3: internal error occurred"))
                          (:DEC_REF_ERROR (error "Z3: Tried to decrement the reference counter of an AST that was deleted or the reference counter was not initialized with Z3_inc_ref."))
                          (:INVALID_USAGE (error "Z3: API call is invalid in the current state: ~a" (z3-get-error-msg ctx error-code)))
                          (:PARSER_ERROR (error "Z3: An error occurred when parsing a string or file: ~a" (z3-get-error-msg ctx error-code)))
                          (:EXCEPTION (error "Z3: An exception occurred: ~a" (z3-get-error-msg ctx error-code)))
                          #| (let ((error-msg (z3-get-error-msg ctx error-code)))
                             (format t "Z3 exception ~S" error-msg)))|#
                          (otherwise (error "Z3: an unknown error occurred with code ~S" error-code)))
                   (ignore-and-continue () :report "Ignore the error and return control to Z3." nil)))

(defun solver-init ()
  (setf *default-context* (make-instance 'context))
  (setf *default-solver* (make-simple-solver *default-context*))
  (z3-set-error-handler *default-context* (cffi:callback error-handler)))

(defun solver-push (&optional solver)
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (z3-solver-push ctx slv)))

(defun solver-pop (&optional solver &key (n 1))
  (let* ((slv (or solver *default-solver*))
         (ctx (get-context slv)))
    (unless (<= n (z3-solver-get-num-scopes ctx slv))
      (error "You can't pop ~S level(s) - the solver is currently at level ~S" n (z3-solver-get-num-scopes ctx slv)))
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
                                   (make-var-decls var-decls ctx)
                                   ctx))))

(defmacro z3-assert (var-decls stmt &optional solver)
  (when (oddp (length var-decls)) (error "Each declared variable must have a type."))
  `(let* ((slv (or ,solver *default-solver*))
          (ctx (get-context slv)))
     (solver-assert slv
                    (convert-to-ast ',stmt
                                    (make-var-decls ',var-decls ctx)
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
           ;; TODO: in the unknown case we may want to get the model and see if the assignment satisfies the assertions
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
