(in-package :z3)

(import 'z3-c-types::(Z3_ast))

;; Create a C array over elements of the given type and with the given length
(defmacro with-foreign-array (array-ty array-len ith-val body)
  `(cffi:with-foreign-object
    (array ',array-ty ,array-len)
    (loop for arg in args
          for i upto (1- ,array-len)
          do (setf (cffi:mem-aref array ',array-ty i) ,ith-val))
    ,body))

(defun convert-to-ast-fn (context stmt &optional types)
  (match stmt
         (t (z3-mk-true context))
         (nil (z3-mk-false context))
         ((satisfies integerp) (z3-mk-numeral context (write-to-string stmt) (z3-mk-int-sort context)))
         ((type symbol) (if (not (assoc stmt types))
                            (error "You must provide types for all variables. You did not for the variable ~S." stmt)
                          (z3-mk-const context (z3-mk-string-symbol context (symbol-name stmt)) (cdr (assoc stmt types)))))
         ((type string) (error "Strings not yet supported."))
         ((list (sym-name fd-val) name val)
          (finite-domain-value-to-ast name val context))
         ((list (sym-name enumval) name val)
          (enum-value-to-ast name val context))
         ((list* (sym-name tuple-val) tuple-name field-values)
          (construct-tuple-fn tuple-name (mapcar (lambda (value) (convert-to-ast-fn context value types)) field-values) context types))
         ((list (sym-name tuple-get) tuple-name field-name value)
          (construct-tuple-field-accessor-fn tuple-name field-name
                                             (convert-to-ast-fn context value types) context))
         ((list* (sym-name bv) args)
          (let ((args
                 (cond ((every #'(lambda (arg) (typep arg 'boolean)) args)
                        args)
                       ((every #'(lambda (arg) (or (eql arg 0) (eql arg 1))) args)
                        (mapcar #'(lambda (arg) (= arg 1)) args))
                       (otherwise (error "You must provide either a list of booleans or a list of (0,1)s to bv.")))))
            (with-foreign-array :bool (length args) arg
                                (z3-mk-bv-numeral context (length args) array))))
         ((list* (sym-name seq) args)
          (assert (plusp (length args)))
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (z3-mk-seq-unit context (convert-to-ast-fn context arg types))
                              (z3-mk-seq-concat context (length args) array)))
         ((list (sym-name seq-empty) sort)
          (z3-mk-seq-empty context (get-sort (list :seq sort) context)))
         ((list (sym-name seq-unit) x)
          (z3-mk-seq-unit context (convert-to-ast-fn context x types)))
         ((type list) (convert-funccall-to-ast context stmt types))
         (otherwise (error "Value ~S is of an unsupported type." stmt))))

(defun convert-to-ast (stmt &optional types context)
  (let* ((ctx (or context *default-context*)))
    (make-instance 'ast
                   :handle (convert-to-ast-fn ctx stmt types)
                   :context ctx)))

;; TODO: arbitrary arity fns should have >=0 args
;; TODO: use a table-like thing similar to what we did in Pete's special topics course
;; so we don't have to list out every variable arity function...
(defun convert-funccall-to-ast (context stmt &optional types)
  (match stmt
         ((list (or '= 'equal '==) x y)
          (z3-mk-eq context
                    (convert-to-ast-fn context x types)
                    (convert-to-ast-fn context y types)))
         ((list 'not arg)
          (z3-mk-not context (convert-to-ast-fn context arg types)))
         ((list* 'and args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-and context (length args) array)))
         ((list* 'or args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-or context (length args) array)))
         ((list* '+ args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-add context (length args) array)))
         ((list* '* args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-mul context (length args) array)))
         ((list '- arg)
          (z3-mk-unary-minus context (convert-to-ast-fn context arg types)))
         ((list* '- args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-sub context (length args) array)))
         ((list* (sym-name distinct) args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-distinct context (length args) array)))
         ((list (sym-name implies) x y)
          (z3-mk-implies context
                         (convert-to-ast-fn context x types)
                         (convert-to-ast-fn context y types)))
         ((list (sym-name xor) x y)
          (z3-mk-xor context
                     (convert-to-ast-fn context x types)
                     (convert-to-ast-fn context y types)))
         ((list 'if test then else)
          (z3-mk-ite context
                    (convert-to-ast-fn context test types)
                    (convert-to-ast-fn context then types)
                    (convert-to-ast-fn context else types)))
         ((list '< x y)
          (z3-mk-lt context
                    (convert-to-ast-fn context x types)
                    (convert-to-ast-fn context y types)))
         ((list '<= x y)
          (z3-mk-le context
                    (convert-to-ast-fn context x types)
                    (convert-to-ast-fn context y types)))
         ((list '> x y)
          (z3-mk-gt context
                    (convert-to-ast-fn context x types)
                    (convert-to-ast-fn context y types)))
         ((list '>= x y)
          (z3-mk-ge context
                    (convert-to-ast-fn context x types)
                    (convert-to-ast-fn context y types)))
         ((list (sym-name bvnot) x)
           (z3-mk-bvnot context
                        (convert-to-ast-fn context x types)))
         ((list (sym-name bvredand) x)
          (z3-mk-bvredand context
                          (convert-to-ast-fn context x types)))
         ((list (sym-name bvredor) x)
          (z3-mk-bvredor context
                         (convert-to-ast-fn context x types)))
         ((list (sym-name bvand) x y)
          (z3-mk-bvand context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvor) x y)
          (z3-mk-bvor context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvxor) x y)
          (z3-mk-bvxor context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvnand) x y)
          (z3-mk-bvnand context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvnor) x y)
          (z3-mk-bvnor context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvxnor) x y)
          (z3-mk-bvxnor context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvneg) x)
          (z3-mk-bvneg context
                       (convert-to-ast-fn context x types)))
         ((list (sym-name bvadd) x y)
          (z3-mk-bvadd context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvsub) x y)
          (z3-mk-bvsub context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvmul) x y)
          (z3-mk-bvmul context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvudiv) x y)
          (z3-mk-bvudiv context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvsdiv) x y)
          (z3-mk-bvsdiv context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvurem) x y)
          (z3-mk-bvurem context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvsmod) x y)
          (z3-mk-bvsmod context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvult) x y)
          (z3-mk-bvult context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvslt) x y)
          (z3-mk-bvslt context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvule) x y)
          (z3-mk-bvule context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvsle) x y)
          (z3-mk-bvule context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvugt) x y)
          (z3-mk-bvugt context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvsgt) x y)
          (z3-mk-bvsgt context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvuge) x y)
          (z3-mk-bvuge context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvsge) x y)
          (z3-mk-bvuge context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
                  ((list (sym-name bvult) x y)
          (z3-mk-bvult context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvslt) x y)
          (z3-mk-bvslt context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvule) x y)
          (z3-mk-bvule context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name concat) x y)
          (z3-mk-concat context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name extract) x hi lo)
          (z3-mk-extract context
                         hi
                         lo
                         (convert-to-ast-fn context x types)))
         ((list (sym-name signext) x len)
          (z3-mk-sign-ext context
                          len
                          (convert-to-ast-fn context x types)))
         ((list (sym-name zeroext) x len)
          (z3-mk-zero-ext context
                          len
                          (convert-to-ast-fn context x types)))
         ((list (sym-name repeat) x maxlen)
          (z3-mk-repeat context
                        maxlen
                        (convert-to-ast-fn context x types)))
         ((list (sym-name bvshl) x y)
          (z3-mk-bvshl context
                       (convert-to-ast-fn context x types)
                       (convert-to-ast-fn context y types)))
         ((list (sym-name bvlshr) x y)
          (z3-mk-bvlshr context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name bvashr) x y)
          (z3-mk-bvashr context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context y types)))
         ((list (sym-name int2bv) x nbits)
          (z3-mk-int2bv context
                        nbits
                        (convert-to-ast-fn context x types)))
         ((list (sym-name bv2int) x signed?)
          (z3-mk-bv2int context
                        (convert-to-ast-fn context x types)
                        signed?))
         ((list* (sym-name seq-concat) args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-seq-concat context (length args) array)))
         ((list (sym-name seq-at) x idx)
          (z3-mk-seq-at context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context idx types)))
         ((list (sym-name seq-nth) x idx)
          (z3-mk-seq-nth context
                        (convert-to-ast-fn context x types)
                        (convert-to-ast-fn context idx types)))
         ((list (sym-name seq-length) x)
          (z3-mk-seq-length context (convert-to-ast-fn context x types)))
         (otherwise (error "Value ~S is of an unsupported type." stmt))))

(defun app-ast-args-to-list (ast ctx)
  (assert (equal (z3-get-ast-kind ctx ast) :app_ast))
  (loop for i below (z3-get-app-num-args ctx ast)
        collect (z3-get-app-arg ctx ast i)))

(defun seq-ast-to-value (ast ctx)
  (assert (equal (z3-get-ast-kind ctx ast) :app_ast))
  (assert (equal (z3-get-sort-kind ctx (z3-get-sort ctx ast)) :seq_sort))
  (let* ((decl (z3-get-app-decl ctx ast))
         (decl-kind (z3-get-decl-kind ctx decl))
         (sort (z3-get-sort ctx ast)))
    (match decl-kind
           (:OP_SEQ_EMPTY nil)
           (:OP_SEQ_UNIT (list (ast-to-value (z3-get-app-arg ctx ast 0))))
           (:OP_SEQ_CONCAT
            (loop for arg in (app-ast-args-to-list ast ctx)
                  append (seq-ast-to-value arg ctx)))
           (otherwise (error "Unsupported operation when trying to convert sequence AST to value: ~S" decl-kind)))))

(defun ast-to-value (ast &optional context)
  (let* ((ctx (or context *default-context*))
         (ast-kind (z3-get-ast-kind ctx ast))
         (sort (z3-get-sort ctx ast))
         (sort-kind (z3-get-sort-kind ctx sort)))
    (match ast-kind
           (:app_ast
            (let* ((decl (z3-get-app-decl ctx (z3-to-app ctx ast))))
              (match (z3-get-decl-kind ctx decl)
                     (:OP_TRUE t)
                     (:OP_FALSE nil)
                     (:OP_DT_CONSTRUCTOR
                      (cond ((enum-sort? sort ctx) (get-enum-value sort decl ctx))
                            ((tuple-sort? sort ctx)
                             (list 'quote (cons (cons :type (sort-name sort ctx))
                                                (loop for field in (get-tuple-fields sort (z3-to-app ctx ast) ctx)
                                                      collect (cons (car field) (ast-to-value (cdr field)))))))
                            (t (error "We don't support custom datatypes like ~S yet." (sort-name sort context)))))
                     ((or :OP_SEQ_CONCAT :OP_SEQ_UNIT :OP_SEQ_EMPTY)
                      (seq-ast-to-value ast ctx))
                     (otherwise (error "Application ASTs for functions with decl-kind ~S are not supported." (z3-get-decl-kind ctx decl))))))
           (:numeral_ast
            (match sort-kind
                   ((or :int_sort :finite_domain_sort :bv_sort) (values (parse-integer (z3-get-numeral-string ctx ast))))
                   (:real_sort (/ (ast-to-value (z3-get-numerator ctx ast) ctx) (ast-to-value (z3-get-denominator ctx ast) ctx)))
                   (otherwise (error "Values with sort kind ~S are not currently supported." sort-kind))))
           (otherwise (error "ASTs of kind ~S are not currently supported." ast-kind)))))

