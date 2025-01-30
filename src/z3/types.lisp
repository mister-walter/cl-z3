;; SPDX-FileCopyrightText: Copyright (c) 2020 Andrew T. Walter <me@atwalter.com>
;; SPDX-License-Identifier: MIT
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
    (let ((str-rep (z3-object-to-string obj)))
      ;; Minor optimization: if the string representation of the
      ;; object has only a single line, keep everything on a single
      ;; line. Otherwise, print the object's representation starting
      ;; on a new line.
      ;; TODO: if multi-line, then indent each line appropriately.
      (when (find #\Newline str-rep :test #'equal)
        (terpri stream))
      (format stream "~A" str-rep))))


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


(defclass func-entry (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v func-entry) (type z3-c-types::func-entry-type))
  (slot-value v 'handle))

;; This type doesn't have a built-in to-string function in Z3, so
;; we had to write something ourselves.
(defmethod z3-object-to-string ((obj func-entry))
  (with-slots (handle context) obj
    (func-entry-to-string handle context)))


;; NOTE: we need to manually increment/decrement reference counter for this type
(defclass func-interp (z3-object-with-handle) ())

(defmethod translate-to-foreign ((v func-interp) (type z3-c-types::func-interp-type))
  (slot-value v 'handle))

;; This type doesn't have a built-in to-string function in Z3, so
;; we'll need to write something ourselves.
(defmethod z3-object-to-string ((obj func-interp))
  (with-slots (handle context) obj
    (let ((s (make-string-output-stream)))
      (format s "{~%")
      (loop for i below (z3-func-interp-get-num-entries context handle)
            do (format s "  ~a,~%" (func-entry-to-string (z3-func-interp-get-entry context handle i) context)))
      (format s "  else -> ~a~%}" (z3-ast-to-string context (z3-func-interp-get-else context handle)))
      (get-output-stream-string s))))

(defmethod initialize-instance :after ((obj func-interp) &key)
  (with-slots (handle context) obj
    (z3-func-interp-inc-ref context handle)
    (tg:finalize obj (lambda () (z3-func-interp-dec-ref context handle)))))


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

(defstruct environment-entry ()
  (tbl (make-hash-table)))

(defmethod print-object ((entry environment-entry) stream)
  (print-unreadable-object (entry stream :type t)
    (with-slots (tbl) entry
      (loop for key being the hash-keys of tbl
            using (hash-value value)
            do (format stream "(~A: ~A)" key (car value))))))

(defmethod env-entry-set (name ty (entry environment-entry))
  (with-slots (tbl) entry
    (setf (gethash name tbl) ty)))

(defmethod env-entry-remove (name (entry environment-entry))
  (with-slots (tbl) entry
    (remhash name tbl)))

;; Create a copy of this env-entry
(defmethod env-entry-copy ((entry environment-entry))
  (with-slots (tbl) entry
    (let ((new-tbl (make-hash-table :size (hash-table-count tbl))))
      (loop for key being the hash-keys of tbl
            using (hash-value value)
            do (setf (gethash key new-tbl) value))
      (make-environment-entry :tbl new-tbl))))

(defclass environment-stack ()
  ((stack :initarg :stack :initform (list (make-environment-entry)) :accessor environment-stack-stack)))

(defmethod print-object ((env environment-stack) stream)
  (print-unreadable-object (env stream :type t)
    (with-slots (stack) env
      (loop for entry in stack
            do (format stream "~%~A" entry)))))

;; Make a copy of this environment, keeping only the top
;; environment-entry but duplicating it.
(defmethod env-flat-copy ((env environment-stack))
  (with-slots (stack) env
    (make-instance 'environment-stack
                   :stack (list (env-entry-copy (car stack))))))

(defmethod env-get (name (env environment-stack))
  (gethash name (environment-entry-tbl (car (environment-stack-stack env)))))

(defmethod env-set (name ty (env environment-stack))
  (multiple-value-bind (existing-ty exists?)
    (env-get name env)
    (cond ((not exists?) (env-entry-set name ty (car (environment-stack-stack env))))
          ((equal existing-ty ty) nil)
          (t (error "Attempting to declare variable ~A with a different type than it is already declared with!" name)))))

;; Remove a binding from the stack, potentially in any entry.
(defmethod env-remove (name (env environment-stack))
  (with-slots (stack) env
    ;; Iterate through the stack starting from the beginning calling
    ;; (env-entry-remove) on each entyr until either we've gone
    ;; through the whole stack or an (env-entry-remove) call results
    ;; in t, indicating that something was removed.
    (loop for entry in stack
          thereis (env-entry-remove name entry))))

;; Remove a binding from only the top entry of the stack
(defmethod env-remove-top (name (env environment-stack))
  (remhash name (environment-entry-tbl (car (environment-stack-stack env)))))

(defmethod env-push ((env environment-stack))
  (let ((new-entry (make-environment-entry)))
    ;; Inherit vars and fns from the previous stack entry
    (loop for key being the hash-keys of (environment-entry-tbl (car (environment-stack-stack env)))
          using (hash-value value)
          do (setf (gethash key (environment-entry-tbl new-entry)) value))
    (push new-entry (environment-stack-stack env))))

(defmethod env-pop ((env environment-stack))
  (pop (environment-stack-stack env)))

(defclass solver-optimize (z3-object-with-handle)
  ((assertion-stack :initform '(()) :accessor solver-assertion-stack)
   (env :initform (make-instance 'environment-stack) :accessor solver-env)))

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
