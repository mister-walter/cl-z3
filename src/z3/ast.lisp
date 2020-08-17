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
         ((list 'fd-val name val)
          (finite-domain-value-to-ast name val context))
         ((list 'enumval name val)
          (enum-value-to-ast name val context))
         ((type list) (convert-funccall-to-ast context stmt types))
         (otherwise (error "Value ~S is of an unsupported type." stmt))))

(defun convert-to-ast (context stmt &optional types)
  (make-instance 'ast
                 :handle (convert-to-ast-fn context stmt types)
                 :context context))

(defmacro with-foreign-array (array-ty array-len ith-val body)
  `(cffi:with-foreign-object
    (array ',array-ty ,array-len)
    (loop for arg in args
          for i upto (1- ,array-len)
          do (setf (cffi:mem-aref array ',array-ty i) ,ith-val))
    ,body))

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
         ((list* 'distinct args)
          (with-foreign-array z3-c-types::Z3_ast (length args)
                              (convert-to-ast-fn context arg types)
                              (z3-mk-distinct context (length args) array)))
         ((list 'implies x y)
          (z3-mk-implies context
                         (convert-to-ast-fn context x types)
                         (convert-to-ast-fn context y types)))
         ((list 'xor x y)
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
         (otherwise (error "Value ~S is of an unsupported type." stmt))))

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
                            (t (error "We don't support custom datatypes like ~S yet." (sort-name sort context)))))
                     (otherwise (error "Application ASTs for functions with decl-kind ~S are not supported." (z3-get-decl-kind ctx decl))))))
           (:numeral_ast
            (match sort-kind
                   #|
                   ;; Pretty sure this is impossible.
                   (:bool_sort
                    (match (z3-get-bool-value ctx ast)
                           (:L_TRUE t)
                           (:L_FALSE nil)
                           (otherwise (error "Tried to get the boolean value of ast ~S but it wasn't true or false!" ast))))
                   |#
                   ((or :int_sort :finite_domain_sort) (values (parse-integer (z3-get-numeral-string ctx ast))))
                   (:real_sort (/ (ast-to-value (z3-get-numerator ctx ast) ctx) (ast-to-value (z3-get-denominator ctx ast) ctx)))
                   (otherwise (error "Values with sort kind ~S are not currently supported." sort-kind))))
           (otherwise (error "ASTs of kind ~S are not currently supported." ast-kind)))))

