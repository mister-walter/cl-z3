(in-package :z3)

(import 'z3-c-types::(Z3_ast))

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
                       (t (error "You must provide either a list of booleans or a list of (0,1)s to bv.")))))
            (with-foreign-array (:bool args arg)
                                (z3-mk-bv-numeral context (length args) array))))
         ((list* (sym-name seq) args)
          (assert (plusp (length args)))
          (with-foreign-array (z3-c-types::Z3_ast
                               args
                               (z3-mk-seq-unit context (convert-to-ast-fn context arg types)))
                              (z3-mk-seq-concat context (length args) array)))
         ((list (sym-name seq-empty) sort)
          (z3-mk-seq-empty context (get-sort (list :seq sort) context)))
         ((list (sym-name seq-unit) x)
          (z3-mk-seq-unit context (convert-to-ast-fn context x types)))
         ((type list) (convert-funccall-to-ast context stmt types))
         (otherwise (error "Value ~S is of an unsupported type." stmt))))

(defun convert-to-ast (stmt types ctx)
  (make-instance 'ast
                 :handle (convert-to-ast-fn ctx stmt types)
                 :context ctx))

;; A partial list of built-in functions.
(defvar *builtin-ops*
  '((not :arity 1)
    (and :arity -)
    (or :arity -)
    (+ :ctor z3-mk-add :arity -)
    (- :ctor z3-mk-sub :arity -)
    ;; note that we special-case unary - by adding a case for it in convert-funccall-to-ast.
    (* :ctor z3-mk-mul :arity -)
    (distinct :arity -)
    (implies :arity 2)
    (xor :arity 2)
    (if :ctor z3-mk-ite :arity 3)
    (< :ctor z3-mk-lt :arity 2)
    (<= :ctor z3-mk-le :arity 2)
    (> :ctor z3-mk-gt :arity 2)
    (>= :ctor z3-mk-ge :arity 2)
    ;; Bitvector functions
    (bvnot :arity 1)
    (bvredand :arity 1)
    (bvredor :arity 1)
    (bvand :arity 2)
    (bvor :arity 2)
    (bvxor :arity 2)
    (bvnand :arity 2)
    (bvnor :arity 2)
    (bvxnor :arity 2)
    (bvneg :arity 1)
    (bvadd :arity 2)
    (bvsub :arity 2)
    (bvmul :arity 2)
    (bvudiv :arity 2)
    (bvsdiv :arity 2)
    (bvurem :arity 2)
    (bvsmod :arity 2)
    (bvult :arity 2)
    (bvslt :arity 2)
    (bvule :arity 2)
    (bvsle :arity 2)
    (bvugt :arity 2)
    (bvsgt :arity 2)
    (bvuge :arity 2)
    (bvsge :arity 2)
    (concat :arity 2)
    ;; extract special
    ;; sign-ext special
    ;; zero-ext special
    ;; repeat special
    (bvshl :arity 2)
    (bvlshr :arity 2)
    (bvashr :arity 2)
    ;; int2bv special
    ;; bv2int special
    ;;; Array functions
    (select :arity 2)
    ;; select_n special
    (store :arity 3)
    ;; store_n special
    ;; const_array special
    ;; map special
    (array-default :arity 1)
    ;; as_array special
    ;;; Special relations
    ;; these are all special
    ))

(defun get-key (k l)
  (cadr (member k l :test #'equal)))

(defmacro mk-op-fn (op)
  (let* ((name (car op))
         (z3-name (or (get-key :ctor (cdr op))
                      (intern (concatenate 'string "Z3-MK-" (symbol-name name)) :z3)))
         (arity (get-key :arity (cdr op))))
    (if (equal arity '-)
        `(lambda (context types &rest args)
           (if (endp args)
               (error "The function ~S must be called with at least one argument." ',name)
             (with-foreign-array (z3-c-types::Z3_ast
                                  args
                                  (convert-to-ast-fn context arg types))
                                 (,z3-name context (length args) array))))
      (let ((arg-names (loop for i below arity collect (gensym))))
        `(lambda (context types ,@arg-names)
           (,z3-name context . ,(mapcar (lambda (arg-name) `(convert-to-ast-fn context ,arg-name types)) arg-names)))))))

(defvar *ops-hash* (make-hash-table :test 'equal))
(loop for op in *builtin-ops*
      for name = (car op)
      do (setf (gethash (symbol-name name) *ops-hash*)
               (eval `(mk-op-fn ,op))))

(defun convert-funccall-to-ast (context stmt &optional types)
  (match stmt
         ((list (or '= 'equal '==) x y)
          (z3-mk-eq context
                    (convert-to-ast-fn context x types)
                    (convert-to-ast-fn context y types)))
         ((list '- arg)
          (z3-mk-unary-minus context (convert-to-ast-fn context arg types)))
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
         ((list (sym-name int2bv) x nbits)
          (z3-mk-int2bv context
                        nbits
                        (convert-to-ast-fn context x types)))
         ((list (sym-name bv2int) x signed?)
          (z3-mk-bv2int context
                        (convert-to-ast-fn context x types)
                        signed?))
         ((list* op args)
          (multiple-value-bind (op-fn exists?)
              (gethash (symbol-name op) *ops-hash*)
            (if exists?
                (apply op-fn context types args)
              (trivia.skip:skip))))
         (otherwise (error "We currently do not support translation of the following expression into Z3.~%~S" stmt))))

(defun app-ast-args-to-list (ast ctx)
  "Get the arguments of an application AST as a list of Z3 AST values."
  (assert (equal (z3-get-ast-kind ctx ast) :app_ast))
  (loop for i below (z3-get-app-num-args ctx ast)
        collect (z3-get-app-arg ctx ast i)))

(defun seq-ast-to-value (ast ctx)
  "Translate a sequence AST into a Lisp list value"
  (assert (equal (z3-get-ast-kind ctx ast) :app_ast))
  (assert (equal (z3-get-sort-kind ctx (z3-get-sort ctx ast)) :seq_sort))
  (let* ((decl (z3-get-app-decl ctx ast))
         (decl-kind (z3-get-decl-kind ctx decl)))
    (match decl-kind
           (:OP_SEQ_EMPTY nil)
           (:OP_SEQ_UNIT (list (ast-to-value (z3-get-app-arg ctx ast 0) ctx)))
           (:OP_SEQ_CONCAT
            (loop for arg in (app-ast-args-to-list ast ctx)
                  append (seq-ast-to-value arg ctx)))
           (otherwise (error "Unsupported operation when trying to convert sequence AST to value: ~S" decl-kind)))))

(defun ast-to-value (ast ctx)
  (let* ((ast-kind (z3-get-ast-kind ctx ast))
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
                                                      collect (cons (car field) (ast-to-value (cdr field) ctx))))))
                            (t (error "We don't support custom datatypes like ~S yet." (sort-name sort ctx)))))
                     ((or :OP_SEQ_CONCAT :OP_SEQ_UNIT :OP_SEQ_EMPTY)
                      (seq-ast-to-value ast ctx))
                     (otherwise (error "Application ASTs for functions with decl-kind ~S are not supported." (z3-get-decl-kind ctx decl))))))
           (:numeral_ast
            (match sort-kind
                   ((or :int_sort :finite_domain_sort :bv_sort) (values (parse-integer (z3-get-numeral-string ctx ast))))
                   (:real_sort (/ (ast-to-value (z3-get-numerator ctx ast) ctx) (ast-to-value (z3-get-denominator ctx ast) ctx)))
                   (otherwise (error "Translation of numeric values with sort kind ~S is not currently supported." sort-kind))))
           (otherwise (error "Translation of ASTs of kind ~S are not currently supported.~%AST that triggered this: ~S" ast-kind (z3-ast-to-string ctx ast))))))

