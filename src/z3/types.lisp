(in-package :z3)

(defclass config ()
  ((handle :initarg :handle
           :initform (z3-mk-config))))

(defmethod translate-to-foreign ((v config) (type z3-c-types::config-type))
  (slot-value v 'handle))

(defclass context ()
  ((handle :initarg :handle
           :initform (z3-mk-context (z3-mk-config)))))

(defmethod translate-to-foreign ((v context) (type z3-c-types::context-type))
  (slot-value v 'handle))

(defclass z3-object-with-handle ()
  ((handle :initarg :handle)
   (context :initarg :context)))

(defgeneric get-context (v)
  (:documentation "Get a context object from another value")
  (:method (v)
           (error "get-context unsupported for values of type ~S" (type-of v)))
  (:method ((v z3-object-with-handle))
           (slot-value v 'context)))

(defgeneric z3-object-to-string (obj)
  (:method ((v z3-object-with-handle))
           (error "You must provide an implementation of the z3-object-to-string generic method for the type ~A" (type-of v))))

(defmethod describe-object ((obj z3-object-with-handle) stream)
  (format stream "~&~A" (z3-object-to-string obj)))

(defmethod print-object ((obj z3-object-with-handle) stream)
  (print-unreadable-object (obj stream :type t)
                           (format stream "~A" (z3-object-to-string obj))))


;; The lifetimes of ast handles are determined by the scope level of solver-push and solver-pop
;; i.e. an ast handle will remain valid until there is a call to solver-pop that takes the current scope below the level where the object was created
(defclass ast (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v ast) (type z3-c-types::ast-type))
  (slot-value v 'handle))

(defmethod translate-to-foreign ((v ast) (type z3-c-types::app-type))
  (unless (z3-is-app (get-context v) v)
    (error "Tried to convert non-app AST ~a into an app value." v))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj ast))
  (with-slots (handle context) obj
    (z3-ast-to-string context handle)))


(defclass func-decl (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v func-decl) (type z3-c-types::func-decl-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj func-decl))
  (with-slots (handle context) obj
    (z3-func-decl-to-string context handle)))


(defclass sort (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v sort) (type z3-c-types::sort-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj sort))
  (with-slots (handle context) obj
    (z3-sort-to-string context handle)))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass model (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v model) (type z3-c-types::model-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj model))
  (with-slots (handle context) obj
    (z3-model-to-string context handle)))

(defmethod initialize-instance :after ((obj model) &key)
  (with-slots (handle context) obj
    (z3-model-inc-ref context handle)
    (tg:finalize obj (lambda () (z3-model-dec-ref context handle)))))

(defclass solver-optimize (z3-object-with-handle)
  ((scopes :initform '(()) :accessor solver-scopes)))

;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass solver (solver-optimize)
  ())

(defmethod translate-to-foreign ((v solver) (type z3-c-types::solver-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj solver))
  (with-slots (handle context) obj
    (format nil "~a" handle)))

(defmethod describe-object ((obj solver) stream)
  (with-slots (handle context) obj
    (format stream "~a" (z3-solver-to-string context handle))))

;; We need this because we have the unset-solver type in
;; globals.lisp. We don't want to call solver-inc-ref in
;; initialize-instance for that class because it doesn't have a real
;; handle value.
(defmethod initialize-instance :after ((obj solver) &key)
  (with-slots (handle context) obj
    (if handle
        (progn (z3-solver-inc-ref context handle)
               (tg:finalize obj (lambda () (z3-solver-dec-ref context handle))))
      (warn "Not incrementing reference count of the solver object because its handle is set to nil."))))

;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass optimizer (solver-optimize)
  ())

(defmethod translate-to-foreign ((v optimizer) (type z3-c-types::optimize-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj optimizer))
  (with-slots (handle context) obj
    (z3-optimize-to-string context handle)))

;; We need this because we have the unset-solver type in
;; globals.lisp. We don't want to call optimize-inc-ref in
;; initialize-instance for that class because it doesn't have a real
;; handle value.
(defmethod initialize-instance :after ((obj optimizer) &key)
  (with-slots (handle context) obj
    (if handle
        (progn (z3-optimize-inc-ref context handle)
               (tg:finalize obj (lambda () (z3-optimize-dec-ref context handle))))
      (warn "Not incrementing reference count of the optimize object because its handle is set to nil."))))



;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass params (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v params) (type z3-c-types::params-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj params))
  (with-slots (handle context) obj
    (z3-params-to-string context handle)))

(defmethod initialize-instance :after ((obj params) &key)
  (with-slots (handle context) obj
    (z3-params-inc-ref context handle)
    (tg:finalize obj (lambda () (z3-params-dec-ref context handle)))))


(defclass param-descrs (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v param-descrs) (type z3-c-types::param-descrs-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj param-descrs))
  (with-slots (handle context) obj
    (z3-param-descrs-to-string context handle)))

(defmethod initialize-instance :after ((obj param-descrs) &key)
  (with-slots (handle context) obj
    (z3-param-descrs-inc-ref context handle)
    (tg:finalize obj (lambda () (format t "Finalizing some param descrs") (z3-param-descrs-dec-ref context handle)))))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass statistics (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v statistics) (type z3-c-types::stats-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj statistics))
  (with-slots (handle context) obj
    (z3-stats-to-string context handle)))

(defmethod initialize-instance :after ((obj statistics) &key)
  (with-slots (handle context) obj
    (z3-stats-inc-ref context handle)
    (tg:finalize obj (lambda () (z3-stats-dec-ref context handle)))))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass tactic (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v tactic) (type z3-c-types::tactic-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj tactic))
  (with-slots (handle context) obj
    (z3-tactic-get-help context handle)))

(defmethod initialize-instance :after ((obj tactic) &key)
  (with-slots (handle context) obj
    (z3-tactic-inc-ref context handle)
    (tg:finalize obj (lambda () (z3-tactic-dec-ref context handle)))))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass ast-vector (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v ast-vector) (type z3-c-types::ast-vector-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj ast-vector))
  (with-slots (handle context) obj
    (z3-ast-vector-to-string context handle)))

(defmethod initialize-instance :after ((obj ast-vector) &key)
  (with-slots (handle context) obj
    (z3-ast-vector-inc-ref context handle)
    (tg:finalize obj (lambda () (z3-ast-vector-dec-ref context handle)))))

(defclass algebraic-number (ast) ())

(defparameter *ALGEBRAIC-NUMBER-PRINT-MODE* :decimal
  "Controls how algebraic numbers are displayed. Default is :decimal. The other option is :root.")
(defparameter *ALGEBRAIC-NUMBER-PRINT-DECIMAL-PRECISION* 4
  "The number of decimal places to include when printing algebraic numbers in :decimal mode.")

(defmethod z3-object-to-string ((obj algebraic-number))
  (with-slots (handle context) obj
    (ecase *ALGEBRAIC-NUMBER-PRINT-MODE*
      (:decimal (z3-get-numeral-decimal-string context handle *ALGEBRAIC-NUMBER-PRINT-DECIMAL-PRECISION*))
      (:root (z3-ast-to-string context handle)))))

(defun make-algebraic-number (context handle)
  (make-instance 'algebraic-number
                 :context context
                 :handle handle))

;; For now, we simply turn the algebraic number into a double. It would
;; be more convenient to use z3-get-numeral-double, but this seems to
;; produce an invalid argument error whenever the value wouldn't fit
;; precisely in a double. This is fair behavior, but probably not what
;; we want here (as the value may be irrational).
(defmethod algebraic-number-to-float ((obj algebraic-number))
  (with-slots (handle context) obj
    (let* ((res (z3-get-numeral-decimal-string context handle *ALGEBRAIC-NUMBER-CONVERT-DECIMAL-PRECISION*))
           (len (length res)))
      (values (parse-float::parse-float res :end (- len 2))))))

#|
(defmethod algebraic-number-to-float ((obj algebraic-number) &key (precision *ALGEBRAIC-NUMBER-DECIMAL-PRECISION*))
  (with-slots (handle context)
      (z3-get-numeral-decimal-string context handle precision)))
|#
