(in-package :z3)

(import 'z3-c-types::(Z3_ast))

(defun make-fn-call (name args types fns ctx)
  (let ((fn-entry (assoc name fns)))
    (unless fn-entry (error "No function with name ~S is known" name))
    (let* ((decl (second fn-entry))
           (arg-sorts-debug (second (third fn-entry)))
           (return-sort-debug (third (third fn-entry))))
      (unless (equal (length arg-sorts-debug) (length args)) (error "Incorrect number of arguments given for function ~S of type ~S" name (third fn-entry)))
      (with-foreign-array (array z3-c-types::Z3_ast args (convert-to-ast-fn ctx arg types fns))
                          (z3-mk-app ctx decl (length args) array)))))

(defun convert-to-ast-fn (context stmt &optional types fns)
  (match stmt
         (t (z3-mk-true context))
         (nil (z3-mk-false context))
         ((satisfies integerp) (z3-mk-numeral context (write-to-string stmt) (z3-mk-int-sort context)))
         ((type symbol) (if (not (assoc stmt types))
                            (error "You must provide types for all variables. You did not for the variable ~S." stmt)
                          (z3-mk-const context (z3-mk-string-symbol context (symbol-name stmt)) (cdr (assoc stmt types)))))
         ((type string) (z3-mk-string context stmt))
         ((list (sym-name unescaped-string) str)
          (assert (stringp str))
          (z3-mk-lstring context (length str) str))
         ((list (sym-name fd-val) name val)
          (finite-domain-value-to-ast name val context))
         ((list (sym-name enumval) name val)
          (enum-value-to-ast name val context))
         ((list* (sym-name tuple-val) tuple-name field-values)
          (construct-tuple-fn tuple-name (mapcar (lambda (value) (convert-to-ast-fn context value types fns)) field-values) context types))
         ((list (sym-name tuple-get) tuple-name field-name value)
          (construct-tuple-field-accessor-fn tuple-name field-name
                                             (convert-to-ast-fn context value types fns) context))
         ((list* (sym-name bv) args)
          (let ((args
                 (cond ((every #'(lambda (arg) (typep arg 'boolean)) args)
                        args)
                       ((every #'(lambda (arg) (or (eql arg 0) (eql arg 1))) args)
                        (mapcar #'(lambda (arg) (= arg 1)) args))
                       (t (error "You must provide either a list of booleans or a list of (0,1)s to bv.")))))
            (with-foreign-array (array :bool args arg)
                                (z3-mk-bv-numeral context (length args) array))))
         ((list* (sym-name seq) args)
          (assert (plusp (length args)))
          (with-foreign-array (array z3-c-types::Z3_ast args
                                     (z3-mk-seq-unit context (convert-to-ast-fn context arg types fns)))
                              (z3-mk-seq-concat context (length args) array)))
         ((list (or (sym-name seq-empty) (sym-name seq.empty)) sort)
          (z3-mk-seq-empty context (get-sort (list :seq sort) context)))
         ((list (or (sym-name seq-unit) (sym-name seq.unit)) x)
          (z3-mk-seq-unit context (convert-to-ast-fn context x types fns)))
         ((list (sym-name re-empty) sort)
          (z3-mk-re-empty context (get-sort sort context)))
         ((list (sym-name re-full) sort)
          (z3-mk-re-full context (get-sort sort context)))
         ((list* (sym-name set) sort args)
          (mk-set (get-sort sort context) args context types fns))
         ((list (sym-name forall) bound-vars body)
          (mk-quantifier t bound-vars body context types fns))
         ((list (sym-name exists) bound-vars body)
          (mk-quantifier nil bound-vars body context types fns))
         ((list* (sym-name _) name args)
          (make-fn-call name args types fns context))
         ((type list) (convert-funccall-to-ast context stmt types fns))
         (otherwise (error "Value ~S is of an unsupported type." stmt))))

;; Given a list of bound variables (e.g. a list consisting of
;; variables, each followed by a type), return (values consts tys)
;; where:
;; - consts is a list containing a Z3 constant for each bound variable
;; - tys is an alist mapping each bound variable to its Z3 sort
(defun process-bound-vars (bound-vars context)
  (let ((consts nil)
        (tys nil))
    (loop for (var ty) on bound-vars by #'cddr while ty
          do (let ((sort (get-sort ty context)))
               (push (z3-mk-const context (z3-mk-string-symbol context (symbol-name var)) sort) consts)
               (push (cons var sort) tys)))
    (values consts tys)))

;; Make a quantifier, given its body and a list of bound variables.
(defun mk-quantifier (is-forall bound-vars body context types fns)
  ;; create a constant for each bound variable
  (multiple-value-bind
    (bound-var-consts bound-types)
    (process-bound-vars bound-vars context)
    ;; move the constants into a C array
    (with-foreign-array (bound-vars z3-c-types::Z3_ast bound-var-consts arg)
                        (z3-mk-quantifier-const context
                                                is-forall
                                                0 ;; weight
                                                (length bound-var-consts)
                                                bound-vars
                                                0 ;; no patterns
                                                (cffi:null-pointer)
                                                ;; the body has access to the bound
                                                ;; variables, so we throw them in with
                                                ;; the existing known variables.
                                                (convert-to-ast-fn context body (append bound-types types) fns)))))

(defun mk-set (sort values ctx types fns)
  (if (endp values)
            (z3-mk-empty-set ctx sort)
      (z3-mk-set-add ctx
                     (mk-set sort (cdr values) ctx types fns)
                     (convert-to-ast-fn ctx (car values) types fns))))

(defun convert-to-ast (stmt types fns ctx)
  (make-instance 'ast
                 :handle (convert-to-ast-fn ctx stmt types fns)
                 :context ctx))

;; A partial list of built-in functions.
;; Each entry should have the following form:
;; (<name/loname> &key :arity :ctor)
;; - <name/loname> is either a symbol or a nonempty list of symbols
;;   corresponding to the symbols that should be translated into this
;;   operator. If :ctor is not provided, the first name is used to
;;   generate the name of the CFFI'ed Z3 function to use to construct
;;   an application AST for this operator, by appending Z3-MK- to the
;;   name.
;; - If :arity is provided, it should either be a positive integer
;;   describing the number of arguments this operator takes, or -
;;   indicating that this operator has arbitrary arity.
;; - If :ctor is provided, it should be a symbol corresponding to the
;;   CFFI'ed Z3 function that constructs an application AST for this
;;   operator.
;; These operator specs will be used to generate lambdas for each
;; operator that take in an S-expression of the appropriate form,
;; convert all arguments into Z3 ASTs recursively, and apply the
;; relevant constructor to those arguments.
(defvar *builtin-ops*
  '((not :arity 1)
    (and :arity -)
    (or :arity -)
    (+ :ctor z3-mk-add :arity -)
    (- :ctor z3-mk-sub :arity -)
    ;; note that we special-case unary - by adding a case for it in convert-funccall-to-ast.
    (* :ctor z3-mk-mul :arity -)
    (/ :ctor z3-mk-div :arity 2)
    (mod :ctor z3-mk-mod :arity 2)
    (rem :ctor z3-mk-rem :arity 2)
    (power :ctor z3-mk-power :arity 2)
    ((int2real int-to-real) :arity 1)
    ((real2int real-to-int) :arity 1)
    ;;(divides :arity 2)
    (distinct :arity -)
    ((implies =>) :arity 2)
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
    (set-has-size :arity 2)
    ;;; Set functions
    ;; mk-empty-set special
    ;; mk-full-set special
    (set-add :arity 2)
    (set-del :arity 2)
    (set-union :arity -)
    (set-intersect :arity -)
    (set-difference :arity 2)
    (set-complement :arity 1)
    (set-member :arity 2)
    (set-subset :arity 2)
    (array-ext :arity 2)
    ;;; Sequence functions
    ;; seq_empty special
    ((seq-unit seq.unit) :arity 1)
    ((seq-concat seq.++ str.++) :arity -)
    ((seq-prefix seq.prefixof str.prefixof) :arity 2)
    ((seq-contains seq.contains str.contains) :arity 2)
    (str-lt :arity 2)
    (str-le :arity 2)
    ((seq-extract seq.extract) :arity 3)
    ((seq-replace seq.replace str.replace) :arity 3)
    ((seq-at seq.at str.at) :arity 2)
    ((seq-nth seq.nth) :arity 2)
    ((seq-length seq.len str.len) :arity 1)
    ((seq-index seq.indexof str.indexof) :arity 3)
    (seq-last-index :arity 2)
    ((str-to-int str.to.int) :arity 1)
    ((int-to-str int.to.str) :arity 1)
    ;;; Regular expression functions
    ((seq-to-re seq.to.re) :arity 1)
    ((seq-in-re seq.in.re) :arity 2)
    ((re-plus re.+) :arity 1)
    ((re-star re.*) :arity 1)
    ((re-option re.opt re.?) :arity 1)
    ((re-union re.union) :arity -)
    ((re-concat re.++) :arity -)
    ((re-range re.range) :arity 2)
    ;; re-loop special
    ((re-intersect re.inter) :arity -)
    (re-complement :arity 1)
    ((re-empty re.empty) :arity 1)
    ((re-full re.full) :arity 1)
    ;;; Special relations
    ;; these are all special
    ))

(defun get-key (k l)
  (cadr (member k l :test #'equal)))

(defmacro mk-op-fn (op)
  (let* ((names (if (consp (car op)) (car op) (list (car op))))
         (name (car names))
         (z3-name (or (get-key :ctor (cdr op))
                      (intern (concatenate 'string "Z3-MK-" (symbol-name name)) :z3)))
         (arity (get-key :arity (cdr op))))
    (if (equal arity '-)
        `(lambda (context types fns &rest args)
           (if (endp args)
               (error "The function ~S must be called with at least one argument." ',name)
             (with-foreign-array (array
                                  z3-c-types::Z3_ast
                                  args
                                  (convert-to-ast-fn context arg types fns))
                                 (,z3-name context (length args) array))))
      (let ((arg-names (loop for i below arity collect (gensym))))
        `(lambda (context types fns ,@arg-names)
           (,z3-name context . ,(mapcar (lambda (arg-name) `(convert-to-ast-fn context ,arg-name types fns)) arg-names)))))))

(defvar *ops-hash* (make-hash-table :test 'equal))
(loop for op in *builtin-ops*
      for names = (if (consp (car op)) (car op) (list (car op)))
      do (loop for name in names
               do (setf (gethash (symbol-name name) *ops-hash*)
                        (eval `(mk-op-fn ,op)))))

(defun convert-funccall-to-ast (context stmt types fns)
  (match stmt
         ((list (or (sym-name =) (sym-name equal) (sym-name ==)) x y)
          (z3-mk-eq context
                    (convert-to-ast-fn context x types fns)
                    (convert-to-ast-fn context y types fns)))
         ((list (sym-name !=) x y)
          (convert-funccall-to-ast context `(not (equal ,x ,y)) types fns))
         ;; Unary subtraction, binary is taken care of thru *builtin-ops*
         ((list (sym-name -) arg)
          (z3-mk-unary-minus context (convert-to-ast-fn context arg types fns)))
         ((list (sym-name extract) hi lo x)
          (z3-mk-extract context
                         hi
                         lo
                         (convert-to-ast-fn context x types fns)))
         ((list (sym-name signext) len x)
          (z3-mk-sign-ext context
                          len
                          (convert-to-ast-fn context x types fns)))
         ((list (sym-name zeroext) len x)
          (z3-mk-zero-ext context
                          len
                          (convert-to-ast-fn context x types fns)))
         ((list (sym-name repeat) maxlen x)
          (z3-mk-repeat context
                        maxlen
                        (convert-to-ast-fn context x types fns)))
         ((list (sym-name int2bv) nbits x)
          (z3-mk-int2bv context
                        nbits
                        (convert-to-ast-fn context x types fns)))
         ((list (sym-name bv2int) x signed?)
          (z3-mk-bv2int context
                        (convert-to-ast-fn context x types fns)
                        signed?))
         ((list (sym-name empty-set) sort)
          (z3-mk-empty-set context (get-sort sort context)))
         ((list (sym-name full-set) sort)
          (z3-mk-full-set context (get-sort sort context)))
         ((list (sym-name re-loop) r lo hi)
          (assert (and (numberp lo) (>= lo 0)))
          (assert (and (numberp hi) (>= hi 0)))
          (z3-mk-re-loop context
                         (convert-to-ast-fn context r types fns)
                         lo hi))
         ;; pseudoboolean constraints
         ((list* (sym-name atmost) args)
          (unless (consp (cdr args)) (error "atmost requires at least 2 arguments"))
          (let ((k (car (last args))))
            (unless (and (integerp k) (>= k 0))
              (error "atmost requires that the last argument is a positive integer"))
            (with-foreign-array (array z3-c-types::Z3_ast (butlast args) (convert-to-ast-fn context arg types fns))
                                (z3-mk-atmost context (1- (length args)) array k))))
         ((list* (sym-name atleast) args)
          (unless (consp (cdr args)) (error "atmost requires at least 2 arguments"))
          (let ((k (car (last args))))
            (unless (and (integerp k) (>= k 0))
              (error "atmost requires that the last argument is a positive integer"))
            (with-foreign-array (array z3-c-types::Z3_ast (butlast args) (convert-to-ast-fn context arg types fns))
                                (z3-mk-atleast context (1- (length args)) array k))))
         ((list* op args)
          (multiple-value-bind (op-fn exists?)
              (gethash (symbol-name op) *ops-hash*)
            (if exists?
                (apply op-fn context types fns args)
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
           ;; TODO: do something better here. For example, this AST
           ;; may represent a string variable.
           (:OP_UNINTERPRETED
            (warn "Handling of OP_UNINTERPRETED is currently a work in progress.")
            (z3-ast-to-string ctx ast))
           (otherwise (error "Unsupported operation when trying to convert sequence AST to value: ~S" decl-kind)))))

;; TODO: this is an experimental feature, don't rely on this switch
;; existing.
(defvar *STRING-REP* :string
  "EXPERIMENTAL.
Controls how strings are represented when converting from the Z3 model
into lisp. Currently there are two modes: :string (the default) and
:list (convert into list of uint8 values)")

(defun get-lstring (context ast)
  (assert (z3-is-string context ast))
  (cffi:with-foreign-object
   (size-ptr :uint 1)
   (let* ((str-ptr (z3-get-lstring context ast size-ptr))
          (size (cffi:mem-ref size-ptr :uint))
          (res-vec (make-array (list size) :element-type '(unsigned-byte 8))))
     (loop for i below size
           do (setf (aref res-vec i) (cffi:mem-aref str-ptr :unsigned-char i)))
     (match *STRING-REP*
            (:string (octets-to-string res-vec :external-format :UTF-8))
            (:list (coerce res-vec 'list))
            (otherwise (error "Unknown string representation mode ~S" *STRING-REP*))))))

(defun assert-app-decl-kind (ctx ast kind)
  (assert (equal (z3-get-ast-kind ctx ast) :app_ast))
  (let* ((decl (z3-get-app-decl ctx (z3-to-app ctx ast)))
         (decl-kind (z3-get-decl-kind ctx decl)))
    (assert (equal decl-kind kind))))

;; Attempt to translate an AST into a Lisp value.
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
                     ;; TODO: do something better here. For example, this AST
                     ;; may represent a string variable.
                     (:OP_UNINTERPRETED
                      (warn "Handling of OP_UNINTERPRETED is currently a work in progress.")
                      (z3-ast-to-string ctx ast))
                     (:OP_ADD
                      (cons '+ (mapcar #'(lambda (arg) (ast-to-value arg ctx)) (app-ast-args-to-list ast ctx))))
                     (:OP_ITE
                      (cons 'if (mapcar #'(lambda (arg) (ast-to-value arg ctx)) (app-ast-args-to-list ast ctx))))
                     (:OP_EQ
                      (cons '= (mapcar #'(lambda (arg) (ast-to-value arg ctx)) (app-ast-args-to-list ast ctx))))
                     (:OP_OR
                      (cons 'or (mapcar #'(lambda (arg) (ast-to-value arg ctx)) (app-ast-args-to-list ast ctx))))
                     ;; Algebraic number
                     (:OP_AGNUM
                      (make-algebraic-number ctx ast))
                     (otherwise
                      ;; TODO fix this ugly special-case
                      (if (z3-is-string ctx ast)
                          ;; TODO: do we want to use get-lstring or get-string here?
                          ;; benefits to using get-lstring: interface
                          ;;   can roundtrip strings, more accurate
                          ;;   representation of model
                          ;; downsides: more annoying to use in a
                          ;;   REPL, unclear how different lisps handle
                          ;;   printing strings with "unprintable"
                          ;;   characters like control codes
                          (get-lstring ctx ast)
                        (error "Translation of application ASTs for functions with decl-kind ~S is not currently supported." (z3-get-decl-kind ctx decl)))))))
           (:numeral_ast
            (match sort-kind
                   ((or :int_sort :finite_domain_sort :bv_sort) (values (parse-integer (z3-get-numeral-string ctx ast))))
                   (:real_sort (/ (ast-to-value (z3-get-numerator ctx ast) ctx) (ast-to-value (z3-get-denominator ctx ast) ctx)))
                   (otherwise (error "Translation of numeric values with sort kind ~S is not currently supported." sort-kind))))
           (:var_ast
            (cons :arg (z3-get-index-value ctx ast)))
           (otherwise (error "Translation of ASTs of kind ~S are not currently supported.~%AST that triggered this: ~S" ast-kind (z3-ast-to-string ctx ast))))))


;; TODO these two functions are very similar - should factor common code into a macro or something.
(defun parse-smt2-string (filename &key sorts decls context)
  "Parse the given string using the SMT-LIB2 parser.
   It returns an ast-vector comprising of the conjunction of assertions in the scope
   (up to push/pop) at the end of the string."
  (let ((ctx (or context *default-context*)))
    (make-instance 'ast-vector
                   :handle
                   (with-foreign-arrays ((sort-names z3-c-types::Z3_symbol sorts (z3-get-sort-name ctx arg))
                                         (z3-sorts z3-c-types::Z3_sort sorts arg)
                                         (decl-names z3-c-types::Z3_symbol decls (z3-get-decl-name ctx arg))
                                         (z3-decls z3-c-types::Z3_func_decl decls arg))
                                        (z3-parse-smtlib2-string ctx filename
                                                               (length sorts) sort-names z3-sorts
                                                               (length decls) decl-names z3-decls))
                   :context ctx)))

;; TODO: use lisp file functions to produce an absolute path here? I
;; have a gut feeling that Z3's file access will not work the same as
;; lisp's in all cases, and that may result in confusion...
(defun parse-smt2-file (filename &key sorts decls context)
  "Parse the file with the given filename  using the SMT-LIB2 parser.
   It returns an ast-vector comprising of the conjunction of assertions in the scope
   (up to push/pop) at the end of the file.
   Calls the error handler if the file does not exist or cannot be accessed."
  (let ((ctx (or context *default-context*)))
    (make-instance 'ast-vector
                   :handle
                   (with-foreign-arrays ((sort-names z3-c-types::Z3_symbol sorts (z3-get-sort-name ctx arg))
                                         (z3-sorts z3-c-types::Z3_sort sorts arg)
                                         (decl-names z3-c-types::Z3_symbol decls (z3-get-decl-name ctx arg))
                                         (z3-decls z3-c-types::Z3_func_decl decls arg))
                                        (z3-parse-smtlib2-file ctx filename
                                                               (length sorts) sort-names z3-sorts
                                                               (length decls) decl-names z3-decls))
                   :context ctx)))
