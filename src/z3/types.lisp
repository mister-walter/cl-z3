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


;; The lifetimes of ast handles are determined by the scope level of solver-push and solver-pop
;; i.e. an ast handle will remain valid until there is a call to solver-pop that takes the current scope below the level where the object was created
(defclass ast ()
  ((handle :initarg :handle)
   (context :initarg :context)))

(defmethod translate-to-foreign ((v ast) (type z3-c-types::ast-type))
  (slot-value v 'handle))

(defmethod describe-object ((v ast) stream)
  (with-slots (handle context) v
    (format stream "~&~A" (z3-ast-to-string context handle))))


(defclass func-decl ()
  ((handle :initarg :handle)
   (context :initarg :context)))

(defmethod translate-to-foreign ((v func-decl) (type z3-c-types::func-decl-type))
  (slot-value v 'handle))

(defmethod describe-object ((v func-decl) stream)
  (with-slots (handle context) v
    (format stream "~&~A" (z3-func-decl-to-string context handle))))


(defclass sort ()
  ((handle :initarg :handle)
   (context :initarg :context)))

(defmethod translate-to-foreign ((v sort) (type z3-c-types::sort-type))
  (slot-value v 'handle))

(defmethod describe-object ((v sort) stream)
  (with-slots (handle context) v
    (format stream "~&~A" (z3-sort-to-string context handle))))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass model ()
  ((handle :initarg :handle)
   (context :initarg :context)))

(defmethod translate-to-foreign ((v model) (type z3-c-types::model-type))
  (slot-value v 'handle))

(defmethod describe-object ((v model) stream)
  (with-slots (handle context) v
    (format stream "~&~A" (z3-model-to-string context handle))))

(defmethod initialize-instance :after ((obj model) &key)
  (with-slots (handle context) obj
    (z3-model-inc-ref context handle)))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass solver ()
  ((handle :initarg :handle)
   (context :initarg :context)))

(defmethod translate-to-foreign ((v solver) (type z3-c-types::solver-type))
  (slot-value v 'handle))

(defmethod describe-object ((v solver) stream)
  (with-slots (handle context) v
    (format stream "~&~A" (z3-solver-to-string context handle))))

(defmethod initialize-instance :after ((obj solver) &key)
  (with-slots (handle context) obj
    (z3-solver-inc-ref context handle)))
