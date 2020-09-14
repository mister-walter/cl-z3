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
    (z3-model-inc-ref context handle)))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass solver (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v solver) (type z3-c-types::solver-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj solver))
  (with-slots (handle context) obj
    (z3-solver-to-string context handle)))

;; We need this because we have the unset-solver type in
;; globals.lisp. We don't want to call solver-inc-ref in
;; initialize-instance for that class because it doesn't have a real
;; solver value.
(defmethod initialize-instance :after ((obj solver) &key)
  (with-slots (handle context) obj
    (if handle
        (z3-solver-inc-ref context handle)
      (warn "Not incrementing reference count of the solver object because its handle is set to nil."))))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass ast-vector (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v ast-vector) (type z3-c-types::ast-vector-type))
  (slot-value v 'handle))

(defmethod z3-object-to-string ((obj ast-vector))
  (with-slots (handle context) obj
    (z3-ast-vector-to-string context handle)))

(defmethod initialize-instance :after ((obj ast-vector) &key)
  (with-slots (handle context) obj
    (z3-ast-vector-inc-ref context handle)))
