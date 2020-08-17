(in-package :z3)

(defvar *sorts* (make-hash-table))

;; Q: Why do we not register the sort value itself?

;; A: Well, the sort value is managed by a context. So we need to
;; ensure that when the context changes, get-sort responds
;; appropriately. It seemed easiest to register (lisp) functions that
;; produce sorts. However, we may need to add arbitrarily many
;; arguments in the future for parametric types (like arrays) unless
;; we want to register specialized versions of each sort.
;; Or maybe we can do so on demand, we'll see.

(defun register-sort (name sort-producer)
  "Register a sort.
   The second argument should be a function object that takes in a single argument (context) and produces a sort."
  (setf (gethash name *sorts*) sort-producer))

;; TODO: optimization - memoize calls within a context
;; i.e. (z3-mk-int-sort ctx) will always return the same value for the same value of ctx (AFAIK unless ctx is reset)
;; (but in that case all pointers that we have into Z3 are invalid so ¯\_(ツ)_/¯)
(defun get-sort (name context)
  "Get the sort associated with a name"
  (multiple-value-bind (fn exists?)
      (gethash name *sorts*)
    (if exists?
        (funcall fn context)
      (error "No known sort with name ~S" name))))

;; Some built-in sorts
(register-sort :int #'z3-mk-int-sort)
(register-sort :bool #'z3-mk-bool-sort)

;;;; Finite domain types

;; WARNING/TODO: When a new context is created after register-finite-domain-sort has been called, the finite domain sorts will NOT exist in the new context.
;; One must make the relevant register-finite-domain-sort calls again.
;; I've tried to check for incorrect usage in a few places, but I'm sure I missed something.

(defstruct finite-domain-sort-metadata
  (sort)
  (size))

(defvar *finite-domain-sort-metadata* (make-hash-table))

(defun register-finite-domain-sort-fn (name size ctx)
  (let ((sort (z3-mk-finite-domain-sort ctx (z3-mk-string-symbol ctx (write-to-string name)) size)))
    (setf (gethash name *finite-domain-sort-metadata*)
          (make-finite-domain-sort-metadata
           :sort sort
           :size size))
    (register-sort name (lambda (context)
                          (if (not (equal context ctx))
                            (error "Attempting to use finite domain type ~S outside of the context in which it is defined.~%You need to call the relevant (register-finite-domain-sort ...) form again in the current context." name)
                            (finite-domain-sort-metadata-sort (gethash name *finite-domain-sort-metadata*)))))))

(defmacro register-finite-domain-sort (name size &optional context)
  `(let ((ctx (or ,context *default-context*)))
     (register-finite-domain-sort-fn ',name ,size ctx)))

(defun finite-domain-value-to-ast (name val context)
  (multiple-value-bind (metadata exists?)
      (gethash name *finite-domain-sort-metadata*)
    (cond ((not exists?) (error "~S does not name a finite-domain sort." name))
          ((not (and (>= val 0) (< val (finite-domain-sort-metadata-size metadata))))
           (error "~S is not a valid member of the finite-domain sort ~S.~%A valid member of this sort is an integer x s.t. 0 <= x < ~A" val name (finite-domain-sort-metadata-size metadata)))
          (t (z3-mk-unsigned-int64 context val (finite-domain-sort-metadata-sort metadata))))))

;;;; Enum types

;; WARNING/TODO: When a new context is created after register-enum-sort has been called, the enum sorts will NOT exist in the new context.
;; One must make the relevant register-enum-sort calls again.
;; I've tried to check for incorrect usage in a few places, but I'm sure I missed something.

(defstruct enum-sort-metadata
  (sort)
  (names)
  (consts)
  (testers))

(defvar *enum-sort-metadata* (make-hash-table))

(defun foreign-array-to-list (arr ty len)
  "Convert a foreign array of the given element type and length into a list."
  (loop for i below len
        collect (cffi:mem-aref arr ty i)))

(defmacro write-to-foreign-array (foreign-array element-type length lisp-list set-ith &optional (elt-var 'elt) (idx-var 'i))
  `(loop for ,elt-var in ,lisp-list
         for ,idx-var below ,length
         do (setf (cffi:mem-aref ,foreign-array ,element-type ,idx-var)
                  ,set-ith)))

(defun register-enum-sort-fn (name elements ctx)
  "Register an enum sort with the given name and elements in the given context."
  (cffi:with-foreign-objects
   ((elt-names 'z3-c-types::Z3_symbol (length elements)) ;; input
    (consts 'z3-c-types::Z3_func_decl (length elements)) ;; output
    (testers 'z3-c-types::Z3_func_decl (length elements))) ;; output
   (loop for elt in elements
         for i below (length elements)
         do (setf (cffi:mem-aref elt-names 'z3-c-types::Z3_symbol i)
                  (z3-mk-string-symbol ctx (write-to-string elt))))
   (let ((sort (z3-mk-enumeration-sort ctx (z3-mk-string-symbol ctx (write-to-string name))
                                       (length elements) elt-names consts testers)))
     (setf (gethash name *enum-sort-metadata*)
           (make-enum-sort-metadata :sort sort
                                    :names elements
                                    :consts (mapcar #'cons elements (foreign-array-to-list consts 'z3-c-types::Z3_func_decl (length elements)))
                                    :testers (mapcar #'cons elements (foreign-array-to-list testers 'z3-c-types::Z3_func_decl (length elements)))))))
  (register-sort name (lambda (context)
                        (if (not (equal context ctx))
                            (error "Attempting to use enumeration type ~S outside of the context in which it is defined.~%You need to call the relevant (register-enum-sort ...) form again in the current context." name)
                          (enum-sort-metadata-sort (gethash name *enum-sort-metadata*))))))

(defmacro register-enum-sort (name elements &optional context)
  `(let ((ctx (or ,context *default-context*)))
     (register-enum-sort-fn ',name ',elements ctx)))

(defun sort-name (sort context)
  "Get the name of a sort."
  (z3-get-symbol-string context (z3-get-sort-name context sort)))

(defun enum-sort? (sort context)
  "Determine if the given sort corresponds to a registered enum sort."
  (let ((sort-name (read-from-string (sort-name sort context))))
    (multiple-value-bind (_ exists?)
        (gethash sort-name *enum-sort-metadata*)
      (declare (ignore _))
      exists?)))

(defun enum-value-func-def (name value)
  "Get the constant func-def corresponding to the given enum sort element."
  (multiple-value-bind (metadata exists?)
      (gethash name *enum-sort-metadata*)
    (cond ((not exists?) (error "~S does not name an enum sort." name))
          ((not (member value (enum-sort-metadata-names metadata)))
           (error "~S is not a member of enum sort ~S.~%Valid values of this sort are ~S." value name (enum-sort-metadata-names metadata)))
          (t (cdr (assoc value (enum-sort-metadata-consts metadata)))))))

(defun enum-value-to-ast (name value ctx)
  "Get an AST node corresponding to the given enum sort element."
  (z3-mk-app ctx (enum-value-func-def name value) 0 (cffi:null-pointer)))

(defun enum-tester-func-def (name value)
  "Get the tester func-def for the given enum sort element."
  (multiple-value-bind (metadata exists?)
      (gethash name *enum-sort-metadata*)
    (cond ((not exists?) (error "~S does not name an enum sort." name))
          ((not (member value (enum-sort-metadata-names metadata)))
           (error "~S is not a member of enum sort ~S.~%Valid values of this sort are ~S." value name (enum-sort-metadata-names metadata)))
          (t (cdr (assoc value (enum-sort-metadata-testers metadata)))))))

(defun get-enum-value (sort decl context)
  "Given a func-decl corresponding to an element of an enum sort, return the Lisp equivalent of the enum element."
  ;; decl must be a func-decl
  (let* ((sort-name (read-from-string (sort-name sort context))))
    (multiple-value-bind (metadata exists?)
        (gethash sort-name *enum-sort-metadata*)
      (if (not exists?)
          (error "Tried to get enum value of non-enum sort ~S" sort-name)
        (car (rassoc decl (enum-sort-metadata-consts metadata)))))))

