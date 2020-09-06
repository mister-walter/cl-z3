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

;; TODO: currently storing these in a separate hash table, but there's
;; really no reason why we can't have a unified hash table of objects
;; that contain the neccesary metadata for each kind of sort that
;; needs it.
(defvar *parametric-sorts* (make-hash-table))

(defun register-parametric-sort (name parametric-sort-producer)
  "Register a sort.
   The second argument should be a function object that takes in two arguments (context, list of user-provided arguments) and produces a sort."
  (setf (gethash name *parametric-sorts*) parametric-sort-producer))

;; TODO: optimization - memoize calls within a context
;; i.e. (z3-mk-int-sort ctx) will always return the same value for the same value of ctx (AFAIK unless ctx is reset)
;; (but in that case all pointers that we have into Z3 are invalid so ¯\_(ツ)_/¯)
(defun get-sort (name context)
  "Get the sort associated with a name"
  (if (consp name)
      (multiple-value-bind (para-fn para-exists?)
          (gethash (car name) *parametric-sorts*)
        (if para-exists?
            (funcall para-fn context (cdr name))
          (multiple-value-bind (fn exists?)
              (gethash name *sorts*)
            (if exists?
                (funcall fn context)
              (error "No known sort with name ~S" name)))))
    (multiple-value-bind (fn exists?)
              (gethash name *sorts*)
            (if exists?
                (funcall fn context)
              (error "No known sort with name ~S" name)))))

;; Some built-in sorts
(register-sort :int #'z3-mk-int-sort)
(register-sort :bool #'z3-mk-bool-sort)
(register-sort :string #'z3-mk-string-sort)

(register-parametric-sort :bv
                          #'(lambda (ctx args)
                              (cond ((not (equal (length args) 1)) (error "bv type only takes a single argument."))
                                    ((or (not (numberp (car args))) (minusp (car args))) (error "bv type must have a positive integer size"))
                                    (t (z3-mk-bv-sort ctx (car args))))))

(register-parametric-sort :seq
                          #'(lambda (ctx args)
                              (if (not (equal (length args) 1))
                                  (error "seq type only takes a single argument.")
                                (z3-mk-seq-sort ctx (get-sort (car args) ctx)))))

(register-parametric-sort :regex
                          #'(lambda (ctx args)
                              (unless (equal (length args) 1)
                                (error "regex type only takes a single argument."))
                              (let ((sort (get-sort (car args) ctx)))
                                (unless (z3-is-seq-sort ctx sort) (error "Regex type must be over a sequence sort."))
                                (z3-mk-re-sort ctx sort))))

(register-parametric-sort :array
                          #'(lambda (ctx args)
                              (unless (equal (length args) 2) (error "array type takes two arguments: domain and range sorts"))
                              (z3-mk-array-sort ctx (get-sort (car args) ctx) (get-sort (second args) ctx))))



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

;;;; Named tuple types

;; WARNING/TODO: When a new context is created after register-tuple-sort has been called, the tuple sorts will NOT exist in the new context.
;; One must make the relevant register-tuple-sort calls again.
;; I've tried to check for incorrect usage in a few places, but I'm sure I missed something.

(defstruct tuple-sort-metadata
  (sort)
  (field-names)
  (field-sorts)
  (constructor)
  (field-accessors))

(defvar *tuple-sort-metadata* (make-hash-table))

(defun register-tuple-sort-fn (name fields ctx)
  "Register an enum sort with the given name and elements in the given context."
  ;; fields should be an alist of symbol->sortlike
  (let ((n-fields (length fields)))
    (cffi:with-foreign-objects
     ((field-names 'z3-c-types::Z3_symbol n-fields) ;; input
      (field-sorts 'z3-c-types::Z3_sort n-fields)
      (constructor-decl 'z3-c-types::Z3_func_decl) ;; output
      (projection-decls 'z3-c-types::Z3_func_decl n-fields)) ;; output
     (loop for (field-name . field-sort) in fields
           for i below n-fields
           do (setf (cffi:mem-aref field-names 'z3-c-types::Z3_symbol i)
                    (z3-mk-string-symbol ctx (write-to-string field-name))) ;; TODO maybe name the fields more like `,name/,field-name`
           do (setf (cffi:mem-aref field-sorts 'z3-c-types::Z3_sort i)
                    (get-sort field-sort ctx)))
     (let ((sort (z3-mk-tuple-sort ctx
                                   (z3-mk-string-symbol ctx (write-to-string name))
                                   n-fields
                                   field-names
                                   field-sorts
                                   constructor-decl
                                   projection-decls)))
       (setf (gethash name *tuple-sort-metadata*)
             (make-tuple-sort-metadata :sort sort
                                       :field-names (mapcar #'car fields)
                                       :field-sorts (foreign-array-to-list field-sorts 'z3-c-types::Z3_sort n-fields) ;; TODO why not just make this list earlier and use it to build the foreign array?
                                       :constructor (cffi:mem-ref constructor-decl 'z3-c-types::Z3_func_decl)
                                       :field-accessors (mapcar #'cons (mapcar #'car fields) (foreign-array-to-list projection-decls 'z3-c-types::Z3_func_decl n-fields)))))))
  (register-sort name (lambda (context)
                        (if (not (equal context ctx))
                            (error "Attempting to use tuple type ~S outside of the context in which it is defined.~%You need to call the relevant (register-tuple-sort ...) form again in the current context." name)
                          (tuple-sort-metadata-sort (gethash name *tuple-sort-metadata*))))))

(defmacro register-tuple-sort (name fields &optional context)
  `(let ((ctx (or ,context *default-context*)))
     (register-tuple-sort-fn ',name ',fields ctx)))

(defun tuple-sort? (sort context)
  "Determine if the given sort corresponds to a registered tuple sort."
  (let ((sort-name (read-from-string (sort-name sort context))))
    (multiple-value-bind (_ exists?)
        (gethash sort-name *tuple-sort-metadata*)
      (declare (ignore _))
      exists?)))


(defun get-tuple-fields (sort app context)
  "Given a application corresponding to the construction of a tuple value, return the AST values of the fields of the"
  (let* ((sort-name (read-from-string (sort-name sort context))))
    (multiple-value-bind (metadata exists?)
        (gethash sort-name *tuple-sort-metadata*)
      (cond ((not exists?)
             (error "Tried to get tuple value of non-tuple sort ~S" sort-name))
            ((not (equal (length (tuple-sort-metadata-field-names metadata))
                         (z3-get-app-num-args context app))) (error "Incorrect number of arguments passed to tuple constructor."))
            (t (loop for name in (tuple-sort-metadata-field-names metadata)
                     for i below (length (tuple-sort-metadata-field-names metadata))
                     collect (cons name (z3-get-app-arg context app i))))))))

(defmacro with-foreign-array-from-list (name array-ty list &rest body)
  `(let ((l ,list))
     (cffi:with-foreign-object
      (,name ',array-ty (length l))
      (loop for elt in l
            for i below (length l)
            do (setf (cffi:mem-aref ,name ',array-ty i) elt))
      ,@body)))

(defun construct-tuple-fn (tuple-name values context &optional types)
  "Make an AST node that constructs a value of the given tuple with the given field values.
   Field values must be provided in the same order as they were defined in the register-tuple-sort call for this tuple sort."
  ;; TODO: write a version of this function that takes in an alist of field-name -> field-value
  ;; such a function would be able to catch errors more effectively
  (multiple-value-bind (metadata exists?)
      (gethash tuple-name *tuple-sort-metadata*)
    (cond ((not exists?) (error "~S does not name a tuple sort." tuple-name))
          ((not (equal (length values) (length (tuple-sort-metadata-field-names metadata))))
           (error "Incorrect number of arguments provided to constructor for ~S: ~S provided, ~S required."
                  tuple-name (length values) (length (tuple-sort-metadata-field-names metadata))))
          (t (with-foreign-array-from-list values-array z3-c-types::Z3_ast values
                                           (z3-mk-app context (tuple-sort-metadata-constructor metadata) (length values) values-array))))))

(defun get-tuple-field-accessor-decl-fn (tuple-name field-name context)
  (multiple-value-bind (metadata exists?)
      (gethash tuple-name *tuple-sort-metadata*)
    (cond ((not exists?) (error "~S does not name a tuple sort." tuple-name))
          ((not (member field-name (tuple-sort-metadata-field-names metadata)))
           (error "Tuple ~S does not contain a field with name ~S.~%Valid field names are ~S." tuple-name field-name (tuple-sort-metadata-field-names metadata)))
          (t (cdr (assoc field-name (tuple-sort-metadata-field-accessors metadata)))))))

(defun construct-tuple-field-accessor-fn (tuple-name field-name tuple-value context)
  "Make an AST node that accesses the given field of the given tuple-value."
  (with-foreign-array-from-list args-array z3-c-types::Z3_ast
                                (list tuple-value)
                                (z3-mk-app context (get-tuple-field-accessor-decl-fn tuple-name field-name context) 1 args-array)))
