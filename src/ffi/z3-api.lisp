#|
(pushnew (truename "/home/drew/lisp-z3/") ql:*local-project-directories* )
(ql:register-local-projects)
(ql:quickload :lisp-z3)
|#

(in-package :z3-c)

;; When a Z3 module is initialized it will use the value of these parameters
;; when Z3_params objects are not provided.

;; The name of parameter can be composed of characters [a-z][A-Z], digits [0-9], '-' and '_'.
;; The character '.' is a delimiter (more later).

;; The parameter names are case-insensitive. The character '-' should be viewed as an "alias" for '_'.
;; Thus, the following parameter names are considered equivalent: "pp.decimal-precision"  and "PP.DECIMAL_PRECISION".

;; This function can be used to set parameters for a specific Z3 module.
;; This can be done by using <module-name>.<parameter-name>.
;; For example:
;; Z3_global_param_set('pp.decimal', 'true')
;; will set the parameter "decimal" in the module "pp" to true.
(defcfun "Z3_global_param_set" :void
  "Set a global (or module) parameter.
   This setting is shared by all Z3 contexts."
  (param_id :string)
  (param_value :string))

(defcfun "Z3_global_param_reset_all" :void
  "Restore the value of all global (and module) parameters.
   This command will not affect already created objects (such as tactics and solvers).")

;; This function cannot be invoked simultaneously from different threads without synchronization.
;; The result string stored in param_value is stored in shared location.
(defcfun "Z3_global_param_get" :bool
  "Get a global (or module) parameter.
   Returns false if the parameter value does not exist."
  (param_id :string)
  (param_value :pointer))

(defconstant +ptr-size+ (foreign-type-size :pointer))

(with-foreign-pointer (ptr +ptr-size+)
                      (let ((status (z3-global-param-get "timeout" ptr)))
                        (if status
                            (mem-ref ptr :string)
                          (error "Unknown parameter"))))

(defcfun "Z3_mk_config" config
  "Create a configuration object for the Z3 context object.
   Configurations are created in order to assign parameters prior to creating
   contexts for Z3 interaction.")

(defcfun "Z3_del_config" :void
  "Delete the given configuration object."
  (c config))

(defcfun "Z3_set_param_value" :void
  "Set a configuration parameter."
  (c config)
  (param_id :string)
  (param_value :string))

#|
In contrast to #Z3_mk_context_rc, the life time of \c Z3_ast objects
are determined by the scope level of #Z3_solver_push and #Z3_solver_pop.
In other words, a \c Z3_ast object remains valid until there is a
call to #Z3_solver_pop that takes the current scope below the level where
the object was created.

Note that all other reference counted objects, including \c Z3_model,
\c Z3_solver, \c Z3_func_interp have to be managed by the caller.
Their reference counts are not handled by the context.

Further remarks:
- \c Z3_sort, \c Z3_func_decl, \c Z3_app, \c Z3_pattern are \c Z3_ast's.
- Z3 uses hash-consing, i.e., when the same \c Z3_ast is created twice,
Z3 will return the same pointer twice.
|#
(defcfun "Z3_mk_context" context
  "Create a context using the given configuration.
   After a context is created, the configuration cannot be changed,
   although some parameters can be changed using #Z3_update_param_value.
   All main interaction with Z3 happens in the context of a \c Z3_context."
  (c config))

(defcfun "Z3_del_context" :void
  "Delete the given logical context."
  (c context))

(defcfun "Z3_update_param_value" :void
  "Set a value of a context parameter."
  (c context)
  (param_id :string)
  (param_value :string))

(defcfun "Z3_interrupt" :void
  "Interrupt the execution of a Z3 procedure.
   This procedure can be used to interrupt: solvers, simplifiers and tactics."
  (c context))

;; Parameters

;; NOTE: Reference counting must be used to manage parameter sets, even when the Z3_context was
;; created using Z3_mk_context instead of Z3_mk_context_rc.
(defcfun "Z3_mk_params" params
  "Create a Z3 (empty) parameter set."
  (c context))

(defcfun "Z3_params_inc_ref" :void
  "Increment the reference counter of the given parameter set."
  (c context)
  (p params))

(defcfun "Z3_params_dec_ref" :void
  "Decrement the reference counter of the given parameter set."
  (c context)
  (p params))

(defcfun "Z3_params_set_bool" :void
  "Add a Boolean parameter `k` with value `v` to the parameter set `p`."
  (c context)
  (p params)
  (k sym)
  (v :bool))

(defcfun "Z3_params_set_uint" :void
  "Add a unsigned parameter `k` with value `v` to the parameter set `p`."
  (c context)
  (p params)
  (k sym)
  (v :uint))

(defcfun "Z3_params_set_double" :void
  "Add a double parameter `k` with value `v` to the parameter set `p`."
  (c context)
  (p params)
  (k sym)
  (v :double))

(defcfun "Z3_params_set_symbol" :void
  "Add a symbol parameter `k` with value `v` to the parameter set `p`."
  (c context)
  (p params)
  (k sym)
  (v sym))

(defcfun "Z3_params_to_string" :string
  "Convert a parameter set into a string. This function is mainly used
   for printing the contents of a parameter set."
  (c context)
  (p params))

(defcfun "Z3_params_validate" :void
  "Validate the parameter set `p` against the parameter description set `d`.
   The procedure invokes the error handler if `p` is invalid."
  (c context)
  (p params)
  (d param-descrs))

;; Parameter Descriptions

(defcfun "Z3_param_descrs_get_kind" param_kind
  "Return the kind associated with the given parameter name `n`."
  (c context)
  (p param-descrs)
  (n sym))

(defcfun "Z3_param_descrs_size" :uint
  "Return the number of parameters in the given parameter description set."
  (c context)
  (p param-descrs))

(defcfun "Z3_param_descrs_get_name" sym
  "Return the name of the parameter at given index `i`."
  (c context)
  (p param-descrs)
  (i :uint))

(defcfun "Z3_param_descrs_get_documentation" :string
  "Retrieve documentation string corresponding to parameter name `s`."
  (c context)
  (p param-descrs)
  (s sym))

(defcfun "Z3_param_descrs_to_string" :string
  "Convert a parameter description set into a string. This function is
   mainly used for printing the contents of a parameter description
   set."
  (c context)
  (p param-descrs))

;; Symbols

(defcfun "Z3_mk_int_symbol" sym
  "Create a Z3 symbol using an integer.
   NB. Not all integers can be passed to this function.
   The legal range of unsigned integers is 0 to 2^30-1."
  (c context)
  (i :int))

(defcfun "Z3_mk_string_symbol" sym
  "Create a Z3 symbol using a C string."
  (c context)
  (s :string))

;; Sorts

(defcfun "Z3_mk_uninterpreted_sort" sort
  "Create a free (uninterpreted) type using the given name (symbol).
   Two free types are considered the same iff the have the same name."
  (c context)
  (s sym))

(defcfun "Z3_mk_bool_sort" sort
  "Create the Boolean type."
  (c context))

(defcfun "Z3_mk_int_sort" sort
  "Create the integer type.
   This type is not the int type found in programming languages.
   A machine integer can be represented using bit-vectors. The function
   #Z3_mk_bv_sort creates a bit-vector type."
  (c context))

(defcfun "Z3_mk_real_sort" sort
  "Create the real type.
   Note that this type is not a floating point number."
  (c context))

(defcfun "Z3_mk_bv_sort" sort
  "Create a bit-vector type of the given size.
   This type can also be seen as a machine integer.
   The size of the bit-vector type must be greater than zero."
  (c context)
  (sz :uint))

(defcfun "Z3_mk_finite_domain_sort" sort
  "Create a named finite domain sort.
   To create constants that belong to the finite domain,
   use the APIs for creating numerals and pass a numeric
   constant together with the sort returned by this call.
   The numeric constant should be between 0 and the less
   than the size of the domain."
  (c context)
  (name sym)
  (size :uint64))

(defcfun "Z3_mk_array_sort" sort
  "Create an array type."
  (c context)
  (domain sort)
  (range sort))

(defcfun "Z3_mk_array_sort_n" sort
  "Create an array type with `n` arguments."
  (c context)
  (n :uint)
  (domain :pointer) ;; const * sort
  (range sort))

(defcfun "Z3_mk_tuple_sort" sort
  "Create a tuple type.
   A tuple with `n` fields has a constructor and `n` projections.
   This function will also declare the constructor and projection functions."
  (c context)
  (mk_tuple_name sym)
  (num_fields :uint)
  (field_names :pointer) ;; symbol[] of size num_fields
  (field_sorts :pointer) ;; sort[] of size num_fields
  (mk_tuple_decl :pointer) ;; output parameter func_decl*
  (proj_decl :pointer)) ;; output parameter func_decl[] of size at least num_fields

(defcfun "Z3_mk_enumeration_sort" sort
  "Create a enumeration sort.
   An enumeration sort with `n` elements.
   This function will also declare the functions corresponding to the enumerations."
  (c context)
  (name sym)
  (n :uint)
  (enum_names :pointer) ;; const symbol[]
  (enum_consts :pointer) ;; output parameter func_decl[]
  (enum_testers :pointer)) ;; output parameter func_decl[]

(defcfun "Z3_mk_list_sort" sort
  "Create a list sort
   A list sort over `elem_sort`
   This function declares the corresponding constructors and testers for lists."
  (c context)
  (name sym)
  (elem_sort sort)
  (nil_decl :pointer) ;; output parameter func_decl*
  (is_nil_decl :pointer) ;; output parameter func_decl*
  (cons_decl :pointer) ;; output parameter func_decl*
  (is_cons_decl :pointer) ;; output parameter func_decl*
  (head_decl :pointer) ;; output parameter func_decl*
  (tail_decl :pointer)) ;; output parameter func_decl*

(defcfun "Z3_mk_constructor" constructor
  "Create a constructor"
  (c context)
  (name sym)
  (recognizer sym)
  (num_fields :uint)
  (field_names :pointer) ;; const symbol[]
  (sorts :pointer) ;; const sort_opt[]
  (sort_refs :pointer)) ;; unsigned[]

(defcfun "Z3_del_constructor" :void
  "Reclaim memory allocated to constructor."
  (c context)
  (constr constructor))

(defcfun "Z3_mk_datatype" sort
  "Create datatype, such as lists, trees, records, enumerations or unions of records.
   The datatype may be recursive. Return the datatype sort."
  (c context)
  (name sym)
  (num_constructors :uint)
  (constructors :pointer)) ;; in/out parameter constructor[]

(defcfun "Z3_mk_constructor_list" constructor-list
  "Create list of constructors"
  (c context)
  (num_constructors :uint)
  (constructors :pointer)) ;; const constructor[]

(defcfun "Z3_del_constructor_list" :void
  "Reclaim memory allocated for constructor list.
   Each constructor inside the constructor list must be independently reclaimed using #Z3_del_constructor."
  (c context)
  (clist constructor-list))

(defcfun "Z3_mk_datatypes" :void
  "Create mutually recursive datatypes."
  (c context)
  (num_sorts :uint)
  (sort_names :pointer) ;; const symbol[]
  (sorts :pointer) ;; out parameter sort[]
  (constructor_lists :pointer)) ;; in/out parameter constructor_list[]

(defcfun "Z3_query_constructor" :void
  (c context)
  (constr constructor)
  (num_fields :uint)
  (constructor :pointer) ;; out parameter func_decl*
  (tester :pointer) ;; out parameter func_decl*
  (accessors :pointer)) ;; out parameter func_decl[] of size `num_fields`

;; Constants and Applications
(defcfun "Z3_mk_func_decl" func-decl
  "Declare a constant or function."
  (c context)
  (s sym)
  (domain_size :uint)
  (domain :pointer) ;; const sort[]
  (range sort))

(defcfun "Z3_mk_app" ast
  "Create a constant or function application."
  (c context)
  (d func-decl)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

;; Z3_mk_const is a shorthand for:
;; Z3_func_decl d = Z3_mk_func_decl(c, s, 0, 0, ty);
;; Z3_ast n            = Z3_mk_app(c, d, 0, 0);
(defcfun "Z3_mk_const" ast
  "Declare and create a constant."
  (c context)
  (s sym)
  (ty sort))

(defcfun "Z3_mk_fresh_func_decl" func-decl
  "Declare a fresh constant or function.
   Z3 will generate an unique name for this function declaration.
   If `prefix` is different from NULL, then the name generated by Z3 will start with `prefix`.
   If `prefix` is NULL, then it is assumed to be the empty string."
  (c context)
  (prefix :string)
  (domain_size :uint)
  (domain :pointer) ;; const sort[]
  (range sort))

;; This function is a shorthand for:
;; Z3_func_decl d = Z3_mk_fresh_func_decl(c, prefix, 0, 0, ty); Z3_ast n = Z3_mk_app(c, d, 0, 0);
;; If `prefix` is NULL, then it is assumed to be the empty string.
(defcfun "Z3_mk_fresh_const" ast
  "Declare and create a fresh constant."
  (c context)
  (prefix :string)
  (ty sort))

(defcfun "Z3_add_rec_def" :void
  "Define the body of a recursive function."
  (c context)
  (f func-decl)
  (n :uint)
  (args :pointer) ;; ast[]
  (body ast))

;; Propositional Logic and Equality

(defcfun "Z3_mk_true" ast
  "Create an AST node representing true"
  (c context))

(defcfun "Z3_mk_false" ast
  "Create an AST node representing false"
  (c context))

(defcfun "Z3_mk_eq" ast
  "Create an AST node representing `l = r`"
  (c context)
  (l ast)
  (r ast))

(defcfun "Z3_mk_distinct" ast
  "Create an AST node representing `distinct(args[0], ..., args[num_args-1])`"
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

(defcfun "Z3_mk_not" ast
  "Create an AST node representing `not(a)`"
  (c context)
  (a ast))

;; The node \c t1 must have Boolean sort, \c t2 and \c t3 must have the same sort.
;; The sort of the new node is equal to the sort of \c t2 and \c t3.
(defcfun "Z3_mk_ite" ast
  "Create an AST node representing an if-then-else: `ite(t1, t2, t3)`."
  (c context)
  (t1 ast)
  (t2 ast)
  (t3 ast))

(defcfun "Z3_mk_iff" ast
  "Create an AST node representing `t1 iff t2`
   The nodes \c t1 and \c t2 must have Boolean sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_implies" ast
  "Create an AST node representing `t1 implies t2`
   The nodes \c t1 and \c t2 must have Boolean sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_xor" ast
  "Create an AST node representing `t1 xor t2`
   The nodes \c t1 and \c t2 must have Boolean sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_and" ast
  "Create an AST node representing `args[0] oamd ... and args[num_args-1]`
   All arguments must have Boolean sort.
   The number of arguments must be greater than zero."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

(defcfun "Z3_mk_or" ast
  "Create an AST node representing `args[0] or ... or args[num_args-1]`
   All arguments must have Boolean sort.
   The number of arguments must be greater than zero."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

(defcfun "Z3_mk_add" ast
  "Create an AST node representing `args[0] + ... + args[num_args-1]`
   All arguments must have int or real sort.
   The number of arguments must be greater than zero."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

(defcfun "Z3_mk_mul" ast
  "Create an AST node representing `args[0] * ... * args[num_args-1]`
   All arguments must have int or real sort.
   The number of arguments must be greater than zero.
   Note: Z3 has limited support for non-linear arithmetic."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

(defcfun "Z3_mk_sub" ast
  "Create an AST node representing `args[0] - ... - args[num_args-1]`
   All arguments must have int or real sort.
   The number of arguments must be greater than zero."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[]

(defcfun "Z3_mk_unary_minus" ast
  "Create an AST node representing `- arg`
   The arguments must have int or real type."
  (c context)
  (arg ast))

(defcfun "Z3_mk_div" ast
  "Create an AST node representing `arg1 div arg2` 
   The arguments must either both have int type or both have real type.
   If the arguments have int type, then the result type is an int
   type, otherwise the the result type is real."
  (c context)
  (arg1 ast)
  (arg2 ast))

(defcfun "Z3_mk_mod" ast
  "Create an AST node representing `arg1 mod arg2` 
   The arguments must have int type."
  (c context)
  (arg1 ast)
  (arg2 ast))

(defcfun "Z3_mk_rem" ast
  "Create an AST node representing `arg1 rem arg2` 
   The arguments must have int type."
  (c context)
  (arg1 ast)
  (arg2 ast))

(defcfun "Z3_mk_power" ast
  "Create an AST node representing `arg1 ^ arg2` 
   The arguments must have int or real type."
  (c context)
  (arg1 ast)
  (arg2 ast))

(defcfun "Z3_mk_lt" ast
  "Create less than.
   The nodes `t1` and `t2` must have the same sort, and must be int or real."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_le" ast
  "Create less than or equal to.
   The nodes `t1` and `t2` must have the same sort, and must be int or real."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_gt" ast
  "Create greater than.
   The nodes `t1` and `t2` must have the same sort, and must be int or real."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_ge" ast
  "Create greater than or equal to.
   The nodes `t1` and `t2` must have the same sort, and must be int or real."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_divides" ast
  "Create division predicate.
   The nodes `t1` and `t2` must be of integer sort.
   The predicate is true when `t1` divides `t2`. For the predicate to be part of 
   linear integer arithmetic, the first argument `t1` must be a non-zero integer."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_int2real" ast
  "Coerce an integer to a real.
   It follows the semantics prescribed by the SMT-LIB standard.
   The node `t1` must have sort integer."
  (c context)
  (t1 ast))

;; TODO: presumably t1 must have sort real? the z3_api docs don't say this though...
(defcfun "Z3_mk_real2int" ast
  "Coerce a real to an integer.
   The semantics of this function follows the SMT-LIB standard
   for the function to_int"
  (c context)
  (t1 ast))

(defcfun "Z3_mk_is_int" ast
  "Check if a real number is an integer."
  (c context)
  (t1 ast))

;; Bit-vectors

(defcfun "Z3_mk_bvnot" ast
  "Bitwise negation.
   The node `t1` must have a bit-vector sort."
  (c context)
  (t1 ast))

(defcfun "Z3_mk_bvredand" ast
  "Take conjunction of bits in vector, return vector of length 1.
   The node `t1` must have a bit-vector sort."
  (c context)
  (t1 ast))

(defcfun "Z3_mk_bvredor" ast
  "Take disjunction of bits in vector, return vector of length 1.
   The node `t1` must have a bit-vector sort."
  (c context)
  (t1 ast))

(defcfun "Z3_mk_bvand" ast
  "Bitwise and.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvor" ast
  "Bitwise or.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvxor" ast
  "Bitwise exclusive-or.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvnand" ast
  "Bitwise nand.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvnor" ast
  "Bitwise nor.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvxnor" ast
  "Bitwise xnor.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvneg" ast
  "Standard two's complement unary minus.
   The node `t1` must have a bit-vector sort."
  (c context)
  (t1 ast))

(defcfun "Z3_mk_bvadd" ast
  "Standard two's complement addition.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsub" ast
  "Standard two's complement subtraction.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvmul" ast
  "Standard two's complement multiplication.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvudiv" ast
  "Unsigned division.
   It is defined as the `floor` of `t1/t2` if `t2` is
   different from zero. If `t2` is zero, then the result
   is undefined.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsdiv" ast
  "Two's complement signed division.
   It is defined in the following way:
   - The `floor` of `t1/t2` if `t2` is different from zero, and `t1*t2 >= 0`.
   - The `ceiling` of `t1/t2` if `t2` is different from zero, and `t1*t2 < 0`.
   If `t2` is zero, then the result is undefined.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvurem" ast
  "Unsigned remainder.
   It is defined as `t1 - (t1 /u t2) * t2`, where `/u` represents unsigned division.
   If `t2` is zero, then the result is undefined.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsmod" ast
  "Two's complement signed remainder (sign follows divisor).
   If `t2` is zero, then the result is undefined.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvult" ast
  "Unsigned less than.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvslt" ast
  "Two's complement signed less than.
   It abbreviates:
        (or (and (= (extract[|m-1|:|m-1|] t1) bit1)
                (= (extract[|m-1|:|m-1|] t2) bit0))
            (and (= (extract[|m-1|:|m-1|] t1) (extract[|m-1|:|m-1|] t2))
                (bvult t1 t2)))
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvule" ast
  "Unsigned less than or equal to.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsle" ast
  "Two's complement signed less than or equal to.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvuge" ast
  "Unsigned greater than or equal to.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsge" ast
  "Two's complement signed greater than or equal to.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvugt" ast
  "Unsigned greater than.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsgt" ast
  "Two's complement signed greater than.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_concat" ast
  "Concatenate the given bit-vectors.
   The nodes `t1` and `t2` must have (possibly different) bit-vector sorts
   The result is a bit-vector of size `n1+n2`, where `n1` (`n2`) is the size
   of `t1` (`t2`)."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_extract" ast
  "Extract the bits `high` down to `low` from a bit-vector of
   size `m` to yield a new bit-vector of size `n`, where `n = high - low + 1`.
   The node `t1` must have a bit-vector sort."
  (c context)
  (high :uint)
  (low :uint)
  (t1 ast))

(defcfun "Z3_mk_sign_ext" ast
  "Sign-extend of the given bit-vector to the (signed) equivalent bit-vector of
   size `m+i`, where `m` is the size of the given bit-vector.
   The node `t1` must have a bit-vector sort."
  (c context)
  (i :uint)
  (t1 ast))

(defcfun "Z3_mk_zero_ext" ast
  "Extend the given bit-vector with zeros to the (unsigned) equivalent bit-vector 
   of size `m+i`, where `m` is the size of the given bit-vector.
   The node `t1` must have a bit-vector sort."
  (c context)
  (i :uint)
  (t1 ast))

(defcfun "Z3_mk_repeat" ast
  "Repeat the given bit-vector up length `i`.
   The node `t1` must have a bit-vector sort."
  (c context)
  (i :uint)
  (t1 ast))

(defcfun "Z3_mk_bvshl" ast
  "Shift left.
   It is equivalent to multiplication by `2^x` where `x` is the value of the
   third argument.
   NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvlshr" ast
  "Logical shift right.
   It is equivalent to unsigned division by `2^x` where `x` is the
   value of the third argument.
   NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvashr" ast
  "Arithmetic shift right.
   It is like logical shift right except that the most significant
   bits of the result always copy the most significant bit of the
   second argument.
   NB. The semantics of shift operations varies between environments. This
       definition does not necessarily capture directly the semantics of the
       programming language or assembly architecture you are modeling.
   The nodes `t1` and `t2` must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_rotate_left" ast
  "Rotate bits of `t1` to the left `i` times.
   The node `t1` must have a bit-vector sort."
  (c context)
  (i :uint)
  (t1 ast))

(defcfun "Z3_mk_rotate_right" ast
  "Rotate bits of `t1` to the right `i` times.
   The node `t1` must have a bit-vector sort."
  (c context)
  (i :uint)
  (t1 ast))

(defcfun "Z3_mk_ext_rotate_left" ast
  "Rotate bits of `t1` to the left `t2` times.
   The nodes `t1` and `t2`  must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_ext_rotate_right" ast
  "Rotate bits of `t1` to the right `t2` times.
   The nodes `t1` and `t2`  must have the same bit-vector sort."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_int2bv" ast
  "Create an `n` bit bit-vector from the integer argument `t1`.
   The resulting bit-vector has `n` bits, where the i'th bit (counting
   from 0 to `n-1`) is 1 if `(t1 div 2^i) mod 2` is 1.       
   The node `t1` must have integer sort."
  (c context)
  (n :uint)
  (t1 ast))

(defcfun "Z3_mk_bv2int" ast
  "Create an integer from the bit-vector argument `t1`
   If `is_signed` is false, then the bit-vector `t1` is treated as unsigned.
   So the result is non-negative and in the range `[0..2^N-1]`, where N are the number of bits in `t1`.
   If `is_signed` is true, `t1` is treated as a signed bit-vector.    
   The node `t1` must have a bit-vector sort."
  (c context)
  (t1 ast)
  (is_signed :bool))

(defcfun "Z3_mk_bvadd_no_overflow" ast
  "Create a predicate that checks that the bit-wise addition
   of `t1` and `t2` does not overflow.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast)
  (is_signed :bool))

(defcfun "Z3_mk_bvadd_no_underflow" ast
  "Create a predicate that checks that the bit-wise addition
   of `t1` and `t2` does not underflow.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsub_no_overflow" ast
  "Create a predicate that checks that the bit-wise subtraction
   of `t1` and `t2` does not overflow.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvsub_no_underflow" ast
  "Create a predicate that checks that the bit-wise subtraction
   of `t1` and `t2` does not underflow.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast)
  (is_signed :bool))

(defcfun "Z3_mk_bvsdiv_no_overflow" ast
  "Create a predicate that checks that the bit-wise signed division
   of `t1` and `t2` does not overflow.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_mk_bvneg_no_overflow" ast
  "Create a predicate that checks that the bit-wise negation does not 
   overflow when `t1` is interpreted as a signed bit-vector.
   The node `t1` must have a bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast))

(defcfun "Z3_mk_bvmul_no_overflow" ast
  "Create a predicate that checks that the bit-wise multiplication
   of `t1` and `t2` does not overflow.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast)
  (is_signed :bool))

(defcfun "Z3_mk_bvmul_no_underflow" ast
  "Create a predicate that checks that the bit-wise multiplication
   of `t1` and `t2` does not under.
   The nodes `t1` and `t2`  must have the same bit-vector sort.
   The returned node is of sort Bool"
  (c context)
  (t1 ast)
  (t2 ast))

;; Arrays

(defcfun "Z3_mk_select" ast
  "Array read.
   The argument `a` is the array and `i` is the index of the array that gets read.
   The node `a` must have an array sort `[domain -> range]`,
   and `i` must have the sort `domain`.
   The sort of the result is `range`."
  (c context)
  (a ast)
  (i ast))

(defcfun "Z3_mk_select_n" ast
  "n-ary Array read.
   The argument `a` is the array and `idxs` are the indices of the array that gets read."
  (c context)
  (a ast)
  (n :uint)
  (idxs :pointer)) ;; const ast[] of length n

(defcfun "Z3_mk_store" ast
  "Array update.
   The node `a` must have an array sort `[domain -> range]`, `i` must have sort `domain`,
   `v` must have sort `range`. The sort of the result is `[domain -> range]`.
   
   The semantics of this function is given by the theory of arrays described in the SMT-LIB
   standard. See http://smtlib.org for more details.
   
   The result of this function is an array that is equal to `a` (with respect to `select`)
   on all indices except for `i`, where it maps to `v` (and the `select` of `a` with
   respect to `i` may be a different value)."
  (c context)
  (a ast)
  (i ast)
  (v ast))

(defcfun "Z3_mk_store_n" ast
  "n-ary Array update."
  (c context)
  (a ast)
  (n :uint)
  (idxs :pointer) ;; const ast[] of length n
  (v ast))

(defcfun "Z3_mk_const_array" ast
  "Create the constant array.
   The resulting term is an array, such that a `select` on an arbitrary index
   produces the value `v`."
  (c context)
  (domain sort)
  (v ast))

(defcfun "Z3_mk_map" ast
  "Map f on the argument arrays.
   The `n` nodes `args` must be of array sorts `[domain_i -> range_i]`.
   The function declaration `f` must have type `range_1 .. range_n -> range`.
   `v` must have sort range. The sort of the result is `[domain_i -> range]`."
  (c context)
  (f func-decl)
  (n :uint)
  (args :pointer)) ;; const ast[] of length n


(defcfun "Z3_mk_array_default" ast
  "Access the array default value.
   Produces the default range value, for arrays that can be represented as
   finite maps with a default range value."
  (c context)
  (array ast))

(defcfun "Z3_mk_as_array" ast
  "Create array with the same interpretation as a function.
   The array satisfies the property (f x) = (select (_ as-array f) x) 
   for every argument x."
  (c context)
  (f func-decl))

(defcfun "Z3_mk_set_has_size" ast
  "Create predicate that holds if Boolean array `set` has `k` elements set to true."
  (c context)
  (set ast)
  (k ast))

;; Sets

(defcfun "Z3_mk_set_sort" sort
  "Create Set type."
  (c context)
  (ty sort))

(defcfun "Z3_mk_empty_set" ast
  "Create the empty set."
  (c context)
  (domain sort))

(defcfun "Z3_mk_full_set" ast
  "Create the full set."
  (c context)
  (domain sort))

(defcfun "Z3_mk_set_add" ast
  "Add an element to a set.
   The first argument must be a set, the second an element."
  (c context)
  (set ast)
  (elem ast))

(defcfun "Z3_mk_set_del" ast
  "Remove an element from a set.
   The first argument must be a set, the second an element."
  (c context)
  (set ast)
  (elem ast))

(defcfun "Z3_mk_set_union" ast
  "Take the union of a list of sets."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[] of length num_args

(defcfun "Z3_mk_set_intersect" ast
  "Take the intersection of a list of sets."
  (c context)
  (num_args :uint)
  (args :pointer)) ;; const ast[] of length num_args

(defcfun "Z3_mk_set_difference" ast
  "Take the set difference between two sets."
  (c context)
  (arg1 ast)
  (arg2 ast))

(defcfun "Z3_mk_set_complement" ast
  "Take the complement of a set."
  (c context)
  (arg ast))

(defcfun "Z3_mk_set_member" ast
  "Check for set membership.
   The first argument should be an element type of the set."
  (c context)
  (elem ast)
  (set ast))

(defcfun "Z3_mk_set_subset" ast
  "Check for subsetness of sets."
  (c context)
  (arg1 ast)
  (arg2 ast))

(defcfun "Z3_mk_array_ext" ast
  "Create array extensionality index given two arrays with the same sort.
   The meaning is given by the axiom:
   (=> (= (select A (array-ext A B)) (select B (array-ext A B))) (= A B))"
  (c context)
  (arg1 ast)
  (arg2 ast))

;; Numerals

(defcfun "Z3_mk_numeral" ast
  "Create a numeral of a given sort."
  (c context)
  (numeral :string)
  (ty sort))

(defcfun "Z3_mk_real" ast
  "Create a real from a fraction"
  (c context)
  (num :int)
  (den :int))

(defcfun "Z3_mk_int" ast
  "Create a numeral of an int, bit-vector, or finite-domain sort.
   This function can be used to create numerals that fit in a machine integer.
   It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string."
  (c context)
  (v :int)
  (ty sort))

(defcfun "Z3_mk_unsigned_int" ast
  "Create a numeral of an int, bit-vector, or finite-domain sort.
   This function can be used to create numerals that fit in a machine unsigned integer.
   It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string."
  (c context)
  (v :uint)
  (ty sort))

(defcfun "Z3_mk_int64" ast
  "Create a numeral of an int, bit-vector, or finite-domain sort.
   This function can be used to create numerals that fit in a machine int64 integer.
   It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string."
  (c context)
  (v :int64)
  (ty sort))

(defcfun "Z3_mk_unsigned_int64" ast
  "Create a numeral of an int, bit-vector, or finite-domain sort.
   This function can be used to create numerals that fit in a machine uint64 integer.
   It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string."
  (c context)
  (v :uint64)
  (ty sort))

(defcfun "Z3_mk_bv_numeral" ast
  "create a bit-vector numeral from a vector of Booleans."
  (c context)
  (sz :uint)
  (bits :pointer)) ;; const *bool with size = sz

;; Sequences and regular expressions

(defcfun "Z3_mk_seq_sort" sort
  "Create a sequence sort out of the sort for the elements."
  (c context)
  (s sort))

(defcfun "Z3_is_seq_sort" :bool
  "Check if `s` is a sequence sort."
  (c context)
  (s sort))

(defcfun "Z3_get_seq_sort_basis" sort
  "Retrieve basis sort for sequence sort."
  (c context)
  (s sort))

(defcfun "Z3_mk_re_sort" sort
  "reate a regular expression sort out of a sequence sort."
  (c context)
  (seq sort))

(defcfun "Z3_is_re_sort" :bool
  "Check if `s` is a regular expression sort."
  (c context)
  (s sort))

(defcfun "Z3_get_re_sort_basis" sort
  "Retrieve basis sort for regex sort."
  (c context)
  (s sort))

(defcfun "Z3_mk_string_sort" sort
  "Create a sort for 8 bit strings.
This function creates a sort for ASCII strings.
Each character is 8 bits."
  (c context))

(defcfun "Z3_is_string_sort" :bool
  "Check if `s` is a string sort."
  (c context)
  (s sort))

(defcfun "Z3_mk_string" ast
  "Create a string constant out of the string that is passed in."
  (c context)
  (s :string))

(defcfun "Z3_mk_lstring" ast
  "Create a string constant out of the string that is passed in
It takes the length of the string as well to take into account
0 characters. The string is unescaped."
  (c context)
  (len :uint)
  (s :string))

(defcfun "Z3_is_string" :bool
  "Determine if `s` is a string constant."
  (c context)
  (s ast))

(defcfun "Z3_get_string" :string
  "Retrieve the string constant stored in `s`.
\pre  Z3_is_string(c, s)"
  (c context)
  (s ast))

;; Note: we return a pointer because we don't want CFFI's default
;; C-string => Lisp string conversion here.
(defcfun "Z3_get_lstring" :pointer
  "Retrieve the unescaped string constant stored in `s`.
\pre  Z3_is_string(c, s)"
  (c context)
  (s ast)
  (length :pointer)) ;; output argument unsigned int*

(defcfun "Z3_mk_seq_empty" ast
  "Create an empty sequence of the sequence sort `seq`.
\pre seq is a sequence sort."
  (c context)
  (seq sort))

(defcfun "Z3_mk_seq_unit" ast
  "Create a unit sequence of `a`."
  (c context)
  (a ast))

(defcfun "Z3_mk_seq_concat" ast
  "Concatenate sequences.
\pre n > 0"
  (c context)
  (n :uint)
  (args :pointer)) ;; const ast[] of length n

(defcfun "Z3_mk_seq_prefix" ast
  "Check if `prefix` is a prefix of `s`.
\pre prefix and s are the same sequence sorts"
  (c context)
  (prefix ast)
  (s ast))

(defcfun "Z3_mk_seq_suffix" ast
  "Check if `suffix` is a suffix of `s`.
\pre suffix and s are the same sequence sorts"
  (c context)
  (suffix ast)
  (s ast))

(defcfun "Z3_mk_seq_contains" ast
  "Check if `container` contains `containee`.
\pre `container` and `containee` are the same sequence sorts."
  (c context)
  (container ast)
  (containee ast))

(defcfun "Z3_mk_str_lt" ast
  "Check if `s1` is lexicographically strictly less than `s2`.
\pre `s1` and `s2` are strings"
  (c context)
  (s1 ast)
  (s2 ast))

(defcfun "Z3_mk_str_le" ast
  "Check if `s1` is equal to or lexicographically strictly less than `s2`.
\pre `s1` and `s2` are strings"
  (c context)
  (s1 ast)
  (s2 ast))

(defcfun "Z3_mk_seq_extract" ast
  "Extract subsequence starting at `offset` of `length`."
  (c context)
  (s ast)
  (offset ast)
  (length ast))

(defcfun "Z3_mk_seq_replace" ast
  "Replace the first occurrence of `src` with `dst` in `s`."
  (c context)
  (s ast)
  (src ast)
  (dst ast))

(defcfun "Z3_mk_seq_at" ast
  "Retrieve from `s` the unit sequence positioned at position `index`.
The sequence is empty if the index is out of bounds."
  (c context)
  (s ast)
  (index ast))

(defcfun "Z3_mk_seq_nth" ast
  "Retrieve from `s` the element positioned at position `index`.
The function is under-specified if the index is out of bounds."
  (c context)
  (s ast)
  (index ast))

(defcfun "Z3_mk_seq_length" ast
  "Return the length of the sequence `s`."
  (c context)
  (s ast))

(defcfun "Z3_mk_seq_index" ast
  "Return index of first occurrence of `substr` in `s` starting from offset `offset`.
If `s` does not contain `substr`, then the value is -1, if `offset` is the length of `s`, then the value is -1 as well.
The value is -1 if `offset` is negative or larger than the length of `s`."
  (c context)
  (s ast)
  (substr ast)
  (offset ast))

(defcfun "Z3_mk_seq_last_index" ast
  "Return the last occurrence of `substr` in `s`.
If `s` does not contain `substr`, then the value is -1."
  (c context)
  (s ast)
  (substr ast))

(defcfun "Z3_mk_str_to_int" ast
  "Convert string to integer."
  (c context)
  (s ast))

(defcfun "Z3_mk_int_to_str" ast
  "Integer to string conversion."
  (c context)
  (s ast))

;; ... (regular expression stuff)

(defcfun "Z3_mk_seq_to_re" ast
  "Create a regular expression that accepts the sequence `seq`."
  (c context)
  (seq ast))

(defcfun "Z3_mk_seq_in_re" ast
  "Check if `seq` is in the language generated by the regular expression `re`."
  (c context)
  (seq ast)
  (re ast))

(defcfun "Z3_mk_re_plus" ast
  "Create the regular language `re+`"
  (c context)
  (re ast))

(defcfun "Z3_mk_re_star" ast
  "Create the regular language `re*`"
  (c context)
  (re ast))

(defcfun "Z3_mk_re_option" ast
  "Create the regular language `[re]`"
  (c context)
  (re ast))

(defcfun "Z3_mk_re_union" ast
  "Create the union of the regular languages.
   \pre n > 0"
  (c context)
  (n :uint)
  (args :pointer)) ;; const ast[] with length >= n

(defcfun "Z3_mk_re_concat" ast
  "Create the concatenation of the regular languages.
   \pre n > 0"
  (c context)
  (n :uint)
  (args :pointer)) ;; const ast[] with length >= n

(defcfun "Z3_mk_re_range" ast
  "Create the range regular expression over two sequences of length 1."
  (c context)
  (lo ast)
  (hi ast))

(defcfun "Z3_mk_re_loop" ast
  "Create a regular expression loop.
   The supplied regular expression `r` is repeated between `lo` and
   `hi` times.
   The `lo` should be below `hi` with one exception: when supplying
   the value `hi` as 0, the meaning is to repeat the argument `r` at
   least `lo` number of times, and with an unbounded upper bound."
  (c context)
  (r ast)
  (lo :uint)
  (hi :uint))

(defcfun "Z3_mk_re_intersect" ast
  "Create the intersection of the regular languages.
   \pre n > 0"
  (c context)
  (n :uint)
  (args :pointer)) ;; const ast[] with length >= n

(defcfun "Z3_mk_re_complement" ast
  "Create the complement of the regular language `re`."
  (c context)
  (re ast))

(defcfun "Z3_mk_re_empty" ast
  "Create an empty regular expression of sort `re`.
   \pre re is a regular expression sort."
  (c context)
  (re sort))

(defcfun "Z3_mk_re_full" ast
  "Create a universal regular expression of sort `re`.
   \pre re is a regular expression sort."
  (c context)
  (re sort))

;; Special relations

(defcfun "Z3_mk_linear_order" func-decl
  "Create a linear ordering relation over signature `a`.
The relation is identified by the index `id`."
  (c context)
  (a sort)
  (id :uint))

(defcfun "Z3_mk_partial_order" func-decl
  "Create a partial ordering relation over signature `a`.
The relation is identified by the index `id`."
  (c context)
  (a sort)
  (id :uint))

(defcfun "Z3_mk_piecewise_linear_order" func-decl
  "Create a piecewise linear ordering relation over signature `a`.
The relation is identified by the index `id`."
  (c context)
  (a sort)
  (id :uint))

(defcfun "Z3_mk_tree_order" func-decl
  "Create a tree ordering relation over signature `a`.
The relation is identified by the index `id`."
  (c context)
  (a sort)
  (id :uint))

(defcfun "Z3_mk_transitive_closure" func-decl
  "Create transitive closure of binary relation.
\pre f is a binary relation, such that the two arguments have the same sorts.
The resulting relation f+ represents the transitive closure of f."
  (c context)
  (f func-decl))

;; Quantifiers

;; ...

;; Accessors

(defcfun "Z3_get_symbol_kind" symbol_kind
  "Return Z3_INT_SYMBOL if the symbol was constructed
   using #Z3_mk_int_symbol, and Z3_STRING_SYMBOL if the symbol
   was constructed using #Z3_mk_string_symbol."
  (c context)
  (s sym))

(defcfun "Z3_get_symbol_int" :int
  "Return the symbol int value.
   \pre Z3_get_symbol_kind(s) == Z3_INT_SYMBOL"
  (c context)
  (s sym))

(defcfun "Z3_get_symbol_string" :string
  "Return the symbol name.
   \pre Z3_get_symbol_kind(s) == Z3_STRING_SYMBOL
   \warning The returned buffer is statically allocated by Z3. It will
   be automatically deallocated when #Z3_del_context is invoked.
   So, the buffer is invalidated in the next call to Z3_get_symbol_string."
  (c context)
  (s sym))

(defcfun "Z3_get_sort_name" sym
  "Return the sort name as a symbol."
  (c context)
  (d sort))

(defcfun "Z3_get_sort_id" :uint
  "Return a unique identifier for `s`."
  (c context)
  (d sort))

(defcfun "Z3_sort_to_ast" ast
  "Convert a Z3_sort into Z3_ast. This is just type casting."
  (c context)
  (s sort))

(defcfun "Z3_is_eq_sort" :bool
  "Compare sorts."
  (c context)
  (s1 sort)
  (s2 sort))

(defcfun "Z3_get_sort_kind" sort_kind
  "Return the sort kind (e.g., array, tuple, int, bool, etc)."
  (c context)
  (s sort))

(defcfun "Z3_get_bv_sort_size" :uint
  "Return the size of the given bit-vector sort.
\pre Z3_get_sort_kind(c, t) == Z3_BV_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_finite_domain_sort_size" :bool
  "Store the size of the sort in `r`. Return false if the call failed.
That is, Z3_get_sort_kind(s) == Z3_FINITE_DOMAIN_SORT"
  (c context)
  (s sort)
  (r :pointer)) ;; output parameter uint64_t*

(defcfun "Z3_get_array_sort_domain" sort
  "Return the domain of the given array sort.
In the case of a multi-dimensional array, this function returns the sort of the first dimension.
\pre Z3_get_sort_kind(c, s) == Z3_ARRAY_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_array_sort_range" sort
  "Return the range of the given array sort.
\pre Z3_get_sort_kind(c, s) == Z3_ARRAY_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_tuple_sort_mk_decl" func-decl
  "Return the constructor declaration of the given tuple sort.
\pre Z3_get_sort_kind(c, s) == Z3_DATATYPE_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_tuple_sort_num_fields" :uint
  "Return the number of fields of the given tuple sort.
\pre Z3_get_sort_kind(c, s) == Z3_DATATYPE_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_tuple_sort_field_decl" func-decl
  "Return the i-th field declaration (i.e., projection function declaration)
of the given tuple sort.
\pre Z3_get_sort_kind(s) == Z3_DATATYPE_SORT
\pre i < Z3_get_tuple_sort_num_fields(c, s)"
  (c context)
  (s sort)
  (i :uint))

(defcfun "Z3_get_datatype_sort_num_constructors" :uint
  "Return number of constructors for datatype.
\pre Z3_get_sort_kind(c, s) == Z3_DATATYPE_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_datatype_sort_constructor" func-decl
  "Return idx'th constructor.
\pre Z3_get_sort_kind(s) == Z3_DATATYPE_SORT
\pre idx < Z3_get_datatype_sort_num_constructors(c, s)"
  (c context)
  (s sort)
  (idx :uint))

(defcfun "Z3_get_datatype_sort_recognizer" func-decl
  "Return idx'th recognizer.
\pre Z3_get_sort_kind(s) == Z3_DATATYPE_SORT
\pre idx < Z3_get_datatype_sort_num_constructors(c, s)"
  (c context)
  (s sort)
  (idx :uint))

(defcfun "Z3_get_datatype_sort_constructor_accessor" func-decl
  "Return idx_a'th accessor for the idx_c'th constructor.
\pre Z3_get_sort_kind(s) == Z3_DATATYPE_SORT
\pre idx_c < Z3_get_datatype_sort_num_constructors(c, s)
\pre idx_a < Z3_get_domain_size(c, Z3_get_datatype_sort_constructor(c, idx_c))"
  (c context)
  (s sort)
  (idx_c :uint)
  (idx_a :uint))

(defcfun "Z3_get_datatype_update_field" ast
  "Update record field with a value.

This corresponds to the 'with' construct in OCaml.
It has the effect of updating a record field with a given value.
The remaining fields are left unchanged. It is the record
equivalent of an array store (see \sa Z3_mk_store).
If the datatype has more than one constructor, then the update function
behaves as identity if there is a mismatch between the accessor and
constructor. For example ((_ update-field car) nil 1) is nil,
while ((_ update-field car) (cons 2 nil) 1) is (cons 1 nil).

\pre Z3_get_sort_kind(Z3_get_sort(c, s)) == Z3_get_domain(c, field_access, 1) == Z3_DATATYPE_SORT
\pre Z3_get_sort(c, value) == Z3_get_range(c, field_access)"
  (c context)
  (field_access func-decl)
  (s ast)
  (value ast))

(defcfun "Z3_get_relation_arity" :uint
  "Return arity of relation.
   \pre Z3_get_sort_kind(s) == Z3_RELATION_SORT"
  (c context)
  (s sort))

(defcfun "Z3_get_relation_column" sort
  " Return sort at i'th column of relation sort.
    \pre Z3_get_sort_kind(c, s) == Z3_RELATION_SORT
    \pre col < Z3_get_relation_arity(c, s)"
  (c context)
  (s sort)
  (col :uint))

(defcfun "Z3_mk_atmost" ast
  "Pseudo-Boolean relations. Encode p1 + p2 + ... + pn <= k"
  (c context)
  (num_args :uint)
  (args :pointer) ;; input parameter ast[] of length num_args
  (k :uint))

(defcfun "Z3_mk_atleast" ast
  "Pseudo-Boolean relations. Encode p1 + p2 + ... + pn >= k"
  (c context)
  (num_args :uint)
  (args :pointer) ;; input parameter ast[] of length num_args
  (k :uint))

(defcfun "Z3_mk_pble" ast
  "Pseudo-Boolean relations. Encode k1*p1 + k2*p2 + ... + kn*pn <= k"
  (c context)
  (num_args :uint)
  (args :pointer) ;; input parameter ast[] of length num_args
  (coeffs :pointer) ;; input parameter int[] of length num_args
  (k :uint))

(defcfun "Z3_mk_pbge" ast
  "Pseudo-Boolean relations. Encode k1*p1 + k2*p2 + ... + kn*pn >= k"
  (c context)
  (num_args :uint)
  (args :pointer) ;; input parameter ast[] of length num_args
  (coeffs :pointer) ;; input parameter int[] of length num_args
  (k :uint))

(defcfun "Z3_mk_pbeq" ast
  "Pseudo-Boolean relations. Encode k1*p1 + k2*p2 + ... + kn*pn = k"
  (c context)
  (num_args :uint)
  (args :pointer) ;; input parameter ast[] of length num_args
  (coeffs :pointer) ;; input parameter int[] of length num_args
  (k :uint))

(defcfun "Z3_func_decl_to_ast" ast
  "Convert a `Z3_func_decl` into `Z3_ast`. This is just type casting."
  (c context)
  (f func-decl))

(defcfun "Z3_is_eq_func_decl" :bool
  "Compare terms."
  (c context)
  (f1 func-decl)
  (f2 func-decl))

(defcfun "Z3_get_func_decl_id" :uint
  "Return a unique identifier for `f`."
  (c context)
  (f func-decl))

(defcfun "Z3_get_decl_name" sym
  "Return the constant declaration name as a symbol."
  (c context)
  (d func-decl))

(defcfun "Z3_get_decl_kind" decl_kind
  "Return the constant declaration name as a symbol."
  (c context)
  (d func-decl))

(defcfun "Z3_get_domain_size" :uint
  "Return the number of parameters of the given declaration."
  (c context)
  (d func-decl))

;; eliding Z3_get_arity because it's an alias for Z3_get_domain_size

(defcfun "Z3_get_domain" sort
  "Return the sort of the i-th parameter of the given function declaration.
   \pre i < Z3_get_domain_size(d)"
  (c context)
  (d func-decl)
  (i :uint))

(defcfun "Z3_get_range" sort
  "Return the range of the given declaration.
   If \c d is a constant (i.e., has zero arguments), then this
   function returns the sort of the constant."
  (c context)
  (d func-decl))

;; TODO: is this the same as Z3_get_domain_size?
(defcfun "Z3_get_decl_num_parameters" :uint
  "Return the number of parameters associated with a declaration."
  (c context)
  (d func-decl))

(defcfun "Z3_get_decl_parameter_kind" parameter_kind
  "Return the parameter type associated with a declaration.
   \pre idx < Z3_get_decl_num_parameters(d)"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_int_parameter" :int
  "Return the integer value associated with an integer parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_INT"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_double_parameter" :double
  "Return the double value associated with a double parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_DOUBLE"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_symbol_parameter" sym
  "Return the symbol value associated with a symbol parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_SYMBOL"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_sort_parameter" sort
  "Return the sort value associated with a sort parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_SORT"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_ast_parameter" ast
  "Return the expression value associated with an expression parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_AST"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_func_decl_parameter" func-decl
  "Return the expression value associated with an expression parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_FUNC_DECL"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_get_decl_rational_parameter" :string
  "Return the rational value, as a string, associated with a rational parameter.
  \pre Z3_get_decl_parameter_kind(c, d, idx) == Z3_PARAMETER_RATIONAL"
  (c context)
  (d func-decl)
  (idx :uint))

(defcfun "Z3_app_to_ast" ast
  "Convert a `Z3_app` into `Z3_ast`. This is just type casting."
  (c context)
  (a app))

(defcfun "Z3_get_app_decl" func-decl
  "Return the declaration of a constant or function application."
  (c context)
  (a app))

(defcfun "Z3_get_app_num_args" :uint
  "Return the number of argument of an application. If the func-decl
   is an constant, then the number of arguments is 0."
  (c context)
  (a app))

(defcfun "Z3_get_app_arg" ast
  "Return the i-th argument of the given application.
   \pre i < Z3_get_app_num_args(c, a)"
  (c context)
  (a app)
  (i :uint))

(defcfun "Z3_is_eq_ast" :bool
  "Compare terms."
  (c context)
  (t1 ast)
  (t2 ast))

(defcfun "Z3_get_ast_id" :uint
  "Return a unique identifier for `a`."
  #|
        The identifier is unique up to structural equality. Thus, two ast nodes
        created by the same context and having the same children and same function symbols
        have the same identifiers. Ast nodes created in the same context, but having
        different children or different functions have different identifiers.
        Variables and quantifiers are also assigned different identifiers according to
        their structure.
  |#
  (c context)
  (a ast))

(defcfun "Z3_get_ast_hash" :uint
  "Return a hash code for the given AST.
   The hash code is structural. You can use Z3_get_ast_id interchangeably with
   this function."
  (c context)
  (a ast))

(defcfun "Z3_get_sort" sort
  "Return the sort of an AST node.
   The AST node must be a constant, application, numeral, bound variable, or quantifier."
  (c context)
  (a ast))

(defcfun "Z3_is_well_sorted" :bool
  "Return true if the given expression `a` is well sorted."
  (c context)
  (a ast))

(defcfun "Z3_get_bool_value" lbool
  "Return Z3_L_TRUE if `a` is true, Z3_L_FALSE if it is false, and Z3_L_UNDEF otherwise."
  (c context)
  (a ast))

(defcfun "Z3_get_ast_kind" ast_kind
  "Return the kind of the given AST."
  (c context)
  (a ast))

(defcfun "Z3_is_app" :bool
  "Determine whether the given AST is an application."
  (c context)
  (a ast))

(defcfun "Z3_is_numeral_ast" :bool
  "Determine whether the given AST is a numeral."
  (c context)
  (a ast))

(defcfun "Z3_is_algebraic_number" :bool
  "Determine whether the given AST is an algebraic number."
  (c context)
  (a ast))

(defcfun "Z3_to_app" app
  "Convert an `ast` into an APP_AST. This is just type casting.
    \pre \code Z3_get_ast_kind(c, a) == \c Z3_APP_AST \endcode"
  (c context)
  (a ast))

(defcfun "Z3_to_func_decl" func-decl
  "Convert an AST into a FUNC_DECL_AST. This is just type casting.
   \pre \code Z3_get_ast_kind(c, a) == Z3_FUNC_DECL_AST \endcode"
  (c context)
  (a ast))

(defcfun "Z3_get_numeral_string" :string
  "Return numeral value, as a string of a numeric constant term
   \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST"
  (c context)
  (a ast))

(defcfun "Z3_get_numeral_decimal_string" :string
  "Return numeral as a string in decimal notation.
   The result has at most `precision` decimal places.
   \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST || Z3_is_algebraic_number(c, a)"
  (c context)
  (a ast)
  (precision :uint))

(defcfun "Z3_get_numeral_double" :double
  "Return numeral as a double.
   \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST || Z3_is_algebraic_number(c, a)"
  (c context)
  (a ast))

(defcfun "Z3_get_numerator" ast
  "Return the numerator (as a numeral AST) of a numeral AST of sort Real.
   \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST"
  (c context)
  (a ast))

(defcfun "Z3_get_denominator" ast
  "Return the denominator (as a numeral AST) of a numeral AST of sort Real.
   \pre Z3_get_ast_kind(c, a) == Z3_NUMERAL_AST"
  (c context)
  (a ast))

(defcfun "Z3_get_numeral_small" :bool
  "Translate a numeral value into as a pair of 64 bit numbers if the representation fits.
   Return true if the numeral value fits in 64 bit numerals, false otherwise.
   \pre Z3_get_ast_kind(a) == Z3_NUMERAL_AST"
  (c context)
  (a ast)
  (num :pointer) ;; output argument int64*
  (dem :pointer)) ;; output argument int64*

(defcfun "Z3_get_numeral_int" :bool
  "Similar to #Z3_get_numeral_string, but only succeeds if
   the value can fit in a machine int. Return true if the call succeeded.
   \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST"
  (c context)
  (v ast)
  (i :pointer)) ;; output argument int*

(defcfun "Z3_get_numeral_uint" :bool
  "Similar to #Z3_get_numeral_string, but only succeeds if
   the value can fit in a machine unsigned int. Return true if the call succeeded.
   \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST"
  (c context)
  (v ast)
  (i :pointer)) ;; output argument unsigned*

(defcfun "Z3_get_numeral_uint64" :bool
  "Similar to #Z3_get_numeral_string, but only succeeds if
   the value can fit in a machine uint64. Return true if the call succeeded.
   \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST"
  (c context)
  (v ast)
  (i :pointer)) ;; output argument uint64*

(defcfun "Z3_get_numeral_int64" :bool
  "Similar to #Z3_get_numeral_string, but only succeeds if
   the value can fit in a machine int64. Return true if the call succeeded.
   \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST"
  (c context)
  (v ast)
  (i :pointer)) ;; output argument int64*

(defcfun "Z3_get_numeral_rational_int64" :bool
  "Similar to #Z3_get_numeral_string, but only succeeds if
   the value can fit as a rationalnumber as machine int64_t int. Return true if the call succeeded.
   \pre Z3_get_ast_kind(c, v) == Z3_NUMERAL_AST"
  (c context)
  (v ast)
  (num :pointer) ;; output argument int64*
  (den :pointer)) ;; output argument int64*

(defcfun "Z3_get_algebraic_number_lower" ast
  "Return a lower bound for the given real algebraic number.
   The interval isolating the number is smaller than 1/10^precision.
   The result is a numeral AST of sort Real.
   \pre Z3_is_algebraic_number(c, a)"
  (c context)
  (a ast)
  (precision :uint))

(defcfun "Z3_get_algebraic_number_upper" ast
  "Return an upper bound for the given real algebraic number.
   The interval isolating the number is smaller than 1/10^precision.
   The result is a numeral AST of sort Real.
   \pre Z3_is_algebraic_number(c, a)"
  (c context)
  (a ast)
  (precision :uint))

(defcfun "Z3_pattern_to_ast" ast
  "Convert a Z3_pattern into Z3_ast. This is just type casting."
  (c context)
  (p pattern))

(defcfun "Z3_get_pattern_num_terms" :uint
  "Return number of terms in pattern."
  (c context)
  (p pattern))

(defcfun "Z3_get_pattern" ast
  "Return i'th ast in pattern."
  (c context)
  (p pattern)
  (idx :uint))

;; ... (quantifier stuff)

(defcfun "Z3_simplify" ast
  "Interface to simplifier.
   Provides an interface to the AST simplifier used by Z3.
   It returns an AST object which is equal to the argument.
   The returned AST is simplified using algebraic simplification rules,
   such as constant propagation (propagating true/false over logical connectives)."
  (c context)
  (a ast))

(defcfun "Z3_simplify_ex" ast
  "Interface to simplifier.
   Provides an interface to the AST simplifier used by Z3.
   This procedure is similar to `Z3_simplify`, but the behavior of the simplifier
   can be configured using the given parameter set."
  (c context)
  (a ast)
  (p params))

(defcfun "Z3_simplify_get_help" :string
  "Return a string describing all available parameters."
  (c context))

(defcfun "Z3_simplify_get_param_descrs" param-descrs
  "Return the parameter description set for the simplify procedure."
  (c context))

;; Modifiers

(defcfun "Z3_update_term" ast
  "Update the arguments of term `a` using the arguments `args`.
   The number of arguments `num_args` should coincide
   with the number of arguments to `a`.
   If `a` is a quantifier, then num_args has to be 1."
  (c context)
  (a ast)
  (num_args :uint)
  (args :pointer)) ;; const ast[] length >= num_args

(defcfun "Z3_substitute" ast
  "Substitute every occurrence of `from[i]` in `a` with `to[i]`, for `i` smaller than `num_exprs`.
   The result is the new AST. The arrays `from` and `to` must have size `num_exprs`.
   For every `i` smaller than `num_exprs`, we must have that sort of `from[i]` must be equal to sort of `to[i]`."
  (c context)
  (a ast)
  (num_exprs :uint)
  (from :pointer) ;; const ast[] length >= num_args
  (to :pointer)) ;; const ast[] length >= num_args

(defcfun "Z3_substitute_vars" ast
  "Substitute the free variables in `a` with the expressions in `to`.
   For every `i` smaller than `num_exprs`, the variable with de-Bruijn index `i` is replaced with term `to[i]`."
  (c context)
  (a ast)
  (num_exprs :uint)
  (to :pointer)) ;; const ast[] length >= num_exprs

(defcfun "Z3_translate" ast
  "Translate/Copy the AST `a` from context `source` to context `target`.
   AST `a` must have been created using context `source`.
   \pre source != target"
  (source context)
  (a ast)
  (target context))

;; Models

(defcfun "Z3_mk_model" model
  "Create a fresh model object. It has reference count 0."
  (c context))

(defcfun "Z3_model_inc_ref" :void
  "Increment the reference counter of the given model."
  (c context)
  (m model))

(defcfun "Z3_model_dec_ref" :void
  "Decrement the reference counter of the given model."
  (c context)
  (m model))

(defcfun "Z3_model_eval" :bool
  "Evaluate the AST node `t` in the given model.
   Return `true` if succeeded, and store the result in `v`."
  (c context)
  (m model)
  (a ast)
  (model_completion :bool)
  (v :pointer)) ;; output parameter ast*

(defcfun "Z3_model_get_const_interp" ast
  "Return the interpretation (i.e., assignment) of constant `a` in the model `m`.
   Return NULL, if the model does not assign an interpretation for `a`.
   That should be interpreted as: the value of `a` does not matter.
   \pre Z3_get_arity(c, a) == 0"
  (c context)
  (m model)
  (a func-decl))

(defcfun "Z3_model_has_interp" :bool
  "Test if there exists an interpretation (i.e., assignment) for `a` in the model `m`."
  (c context)
  (m model)
  (a func-decl))

(defcfun "Z3_model_get_func_interp" func-interp
  "Return the interpretation of the function `f` in the model `m`.
   Return NULL, if the model does not assign an interpretation for `f`.
   That should be interpreted as: the `f` does not matter.
   \pre Z3_get_arity(c, f) > 0"
  (c context)
  (m model)
  (f func-decl))

(defcfun "Z3_model_get_num_consts" :uint
  "Return the number of constants assigned by the given model."
  (c context)
  (m model))

(defcfun "Z3_model_get_const_decl" func-decl
  "Return the i-th constant in the given model.
   \pre i < Z3_model_get_num_consts(c, m)"
  (c context)
  (m model)
  (i :uint))

(defcfun "Z3_model_get_num_funcs" :uint
  "Return the number of function interpretations in the given model.
   A function interpretation is represented as a finite map and an 'else' value.
   Each entry in the finite map represents the value of a function given a set of arguments."
  (c context)
  (m model))

(defcfun "Z3_model_get_func_decl" func-decl
  "Return the declaration of the i-th function in the given model.
   \pre i < Z3_model_get_num_funcs(c, m)"
  (c context)
  (m model)
  (i :uint))

(defcfun "Z3_model_get_num_sorts" :uint
  "Return the number of uninterpreted sorts that `m` assigns an interpretation to.
   Z3 also provides an interpretation for uninterpreted sorts used in a formula.
   The interpretation for a sort \c s is a finite set of distinct values. We say this finite set is
   the \"universe\" of `s`."
  (c context)
  (m model))

(defcfun "Z3_model_get_sort" sort
  "Return a uninterpreted sort that `m` assigns an interpretation.
   \pre i < Z3_model_get_num_sorts(c, m)"
  (c context)
  (m model)
  (i :uint))

(defcfun "Z3_model_get_sort_universe" ast-vector
  "Return the finite set of distinct values that represent the interpretation for sort `s`."
  (c context)
  (m model)
  (s sort))

(defcfun "Z3_model_translate" model
  "Translate model from context `c` to context `dst`."
  (c context)
  (m model)
  (dst context))

(defcfun "Z3_is_as_array" :bool
  "The \ccode{(_ as-array f)} AST node is a construct for assigning interpretations for arrays in Z3.
   It is the array such that forall indices `i` we have that \ccode{(select (_ as-array f) i)} is equal to \ccode{(f i)}.
   This procedure returns true if the `a` is an `as-array` AST node."
  (c context)
  (a ast))

(defcfun "Z3_get_as_array_func_decl" func-decl
  "Return the function declaration `f` associated with a \ccode{(_ as_array f)} node."
  (c context)
  (a ast))

(defcfun "Z3_add_func_interp" func-interp
  "Create a fresh func_interp object, add it to a model for a specified function.
   It has reference count 0."
  (c context)
  (m model)
  (f func-decl)
  (default-value ast))

(defcfun "Z3_add_const_interp" :void
  "Add a constant interpretation."
  (c context)
  (m model)
  (f func-decl)
  (a ast))

(defcfun "Z3_func_interp_inc_ref" :void
  "Increment the reference counter of the given Z3_func_interp object."
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_dec_ref" :void
  "Decrement the reference counter of the given Z3_func_interp object."
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_get_num_entries" :uint
  "Return the number of entries in the given function interpretation.
   A function interpretation is represented as a finite map and an 'else' value.
   Each entry in the finite map represents the value of a function given a set of arguments.
   This procedure return the number of element in the finite map of `f`."
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_get_entry" func-entry
  "Return a \"point\" of the given function interpretation. It represents the
   value of `f` in a particular point.
   \pre i < Z3_func_interp_get_num_entries(c, f)"
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_get_else" ast
  "Return the 'else' value of the given function interpretation.
   A function interpretation is represented as a finite map and an 'else' value.
   This procedure returns the 'else' value."
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_set_else" :void
  "Set the 'else' value of the given function interpretation.
   A function interpretation is represented as a finite map and an 'else' value.
   This procedure can be used to update the 'else' value."
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_get_arity" :uint
  "Return the arity (number of arguments) of the given function interpretation."
  (c context)
  (f func-interp))

(defcfun "Z3_func_interp_add_entry" :void
  "Add a function entry to a function interpretation.
   It is assumed that entries added to a function cover disjoint arguments.
   If an two entries are added with the same arguments, only the second insertion survives and the
   first inserted entry is removed."
  (c context)
  (fi func-interp)
  (args ast-vector)
  (value ast))

(defcfun "Z3_func_entry_inc_ref" :void
  "Increment the reference counter of the given Z3_func_entry object."
  (c context)
  (e func-entry))

(defcfun "Z3_func_entry_dec_ref" :void
  "Decrement the reference counter of the given Z3_func_entry object."
  (c context)
  (e func-entry))

(defcfun "Z3_func_entry_get_value" ast
  "Return the value of this point.
   A Z3_func_entry object represents an element in the finite map used to encode
   a function interpretation."
  (c context)
  (e func-entry))

(defcfun "Z3_func_entry_get_num_args" :uint
  "Return the number of arguments in a Z3_func_entry object."
  (c context)
  (e func-entry))

(defcfun "Z3_func_entry_get_arg" ast
  "Return an argument of a Z3_func_entry object.
   \pre i < Z3_func_entry_get_num_args(c, e)"
  (c context)
  (e func-entry)
  (i :uint))

;; Interaction logging

(defcfun "Z3_open_log" :bool
  "Log interaction to a file."
  (filename :string))

(defcfun "Z3_append_log" :void
  "Append user-defined string to interaction log.
   The interaction log is opened using Z3_open_log.
   It contains the formulas that are checked using Z3.
   You can use this command to append comments, for instance."
  (string :string))

(defcfun "Z3_close_log" :void
  "Close interaction log.")

(defcfun "Z3_toggle_warning_messages" :void
  "Enable/disable printing warning messages to the console.
   Warnings are printed after passing true, warning messages are
   suppressed after calling this method with false."
  (enabled :bool))

;; String conversion

#|
The default mode for pretty printing AST nodes is to produce
SMT-LIB style output where common subexpressions are printed
at each occurrence. The mode is called \c Z3_PRINT_SMTLIB_FULL.
To print shared common subexpressions only once,
use the \c Z3_PRINT_LOW_LEVEL mode.
To print in way that conforms to SMT-LIB standards and uses let
expressions to share common sub-expressions use \c Z3_PRINT_SMTLIB2_COMPLIANT.
|#
(defcfun "Z3_set_ast_print_mode" :void
  "Select mode for the format used for pretty-printing AST nodes."
  (c context)
  (mode ast_print_mode))

;; \warning The result buffer is statically allocated by Z3. It will
;; be automatically deallocated when #Z3_del_context is invoked.
;; So, the buffer is invalidated in the next call to \c Z3_ast_to_string.

(defcfun "Z3_ast_to_string" :string
  "Convert the given AST node into a string."
  (c context)
  (a ast))

(defcfun "Z3_pattern_to_string" :string
  "Convert the given pattern into a string."
  (c context)
  (p Z3_pattern))

(defcfun "Z3_sort_to_string" :string
  "Convert the given sort into a string."
  (c context)
  (s sort))

(defcfun "Z3_func_decl_to_string" :string
  "Convert the given function declaration into a string."
  (c context)
  (d func-decl))

;; \warning The result buffer is statically allocated by Z3. It will
;; be automatically deallocated when #Z3_del_context is invoked.
;; So, the buffer is invalidated in the next call to \c Z3_model_to_string.
(defcfun "Z3_model_to_string" :string
  "Convert the given model into a string."
  (c context)
  (m model))

#|
\warning The result buffer is statically allocated by Z3. It will
be automatically deallocated when #Z3_del_context is invoked.
So, the buffer is invalidated in the next call to \c Z3_benchmark_to_smtlib_string.
|#
(defcfun "Z3_benchmark_to_smtlib_string" :string
  "Convert the givenbenchmark into SMT-LIB formatted string."
  (c context)
  (name :string) ;; optional
  (logic :string)
  (status :string) ;; sat, unsat, or unknown
  (attributes :string)
  (num_assumptions :uint)
  (assumptions :pointer) ;; const ast[] with length = num_assumptions
  (formula ast))

;; Parser interface
(defcfun "Z3_parse_smtlib2_string" ast-vector
  "Parse the given string using the SMT-LIB2 parser.
   It returns a formula comprising of the conjunction of assertions in the scope
   (up to push/pop) at the end of the string."
  (c context)
  (str :string)
  (num_sorts :uint)
  (sort_names :pointer) ;; const sym[] size = num_sorts
  (sorts :pointer) ;; const sort[] size = num_sorts
  (num_decls :uint)
  (decl_names :pointer) ;; const sym[] size = num_decls
  (decls :pointer)) ;; const func_decl[] size = num_decls

(defcfun "Z3_parse_smtlib2_file" ast-vector
  "Similar to #Z3_parse_smtlib2_string, but reads the benchmark from a file."
  (c context)
  (file_name :string)
  (num_sorts :uint)
  (sort_names :pointer) ;; const sym[] size = num_sorts
  (sorts :pointer) ;; const sort[] size = num_sorts
  (num_decls :uint)
  (decl_names :pointer) ;; const sym[] size = num_decls
  (decls :pointer)) ;; const func_decl[] size = num_decls

(defcfun "Z3_eval_smtlib2_string" :string
  "Parse and evaluate and SMT-LIB2 command sequence. The state from a previous call is saved so the next
   evaluation builds on top of the previous call."
  (c context)
  (str :string))

;; Error Handling

#-z3-safe-errors
(defcfun "Z3_get_error_code" error_code
  "Return the error code for the last API call.
   A call to a Z3 function may return a non Z3_OK error code,
   when it is not used correctly."
  (c context))

#-z3-safe-errors
(defcfun "Z3_set_error_handler" :void
  "Register a Z3 error handler.
   A call to a Z3 function may return a non Z3_OK error code, when
   it is not used correctly.  An error handler can be registered
   and will be called in this case.  To disable the use of the
   error handler, simply register with `h=NULL`.
   \warning Log files, created using #Z3_open_log, may be potentially incomplete/incorrect if error handlers are used."
  (c context)
  (h :pointer)) ;; function pointer of type Z3_error_handler

(defcfun "Z3_set_error" :void
  "Set an error."
  (c context)
  (e error_code))

(defcfun "Z3_get_error_msg" :string
  "Return a string describing the given error code."
  (c context)
  (err error_code))

;; Miscellaneous

;; ...

#|
(defconstant c (z3-mk-config))
(defconstant ctx (z3-mk-context c))
(defconstant a (z3-mk-true ctx))
(z3-ast-to-string ctx a)
|#

;; Goals

#|
If \c models is \c true, then model generation is enabled for the new goal.

If \c unsat_cores is \c true, then unsat core generation is enabled for the new goal.

If \c proofs is \c true, then proof generation is enabled for the new goal. Remark, the
Z3 context \c c must have been created with proof generation support.

\remark Reference counting must be used to manage goals, even when the \c Z3_context was
created using #Z3_mk_context instead of #Z3_mk_context_rc.
|#
(defcfun "Z3_mk_goal" goal
  "Create a goal (aka problem). A goal is essentially a set
   of formulas, that can be solved and/or transformed using
   tactics and solvers."
  (c context)
  (models :bool)
  (unsat_cores :bool)
  (proofs :bool))

(defcfun "Z3_goal_inc_ref" :void
  "Increment the reference counter of the given goal."
  (c context)
  (g goal))

(defcfun "Z3_goal_dec_ref" :void
  "Decrement the reference counter of the given goal."
  (c context)
  (g goal))

(defcfun "Z3_goal_precision" goal_prec
  "Return the \"precision\" of the given goal. Goals can be transformed using over and under approximations.
   A under approximation is applied when the objective is to find a model for a given goal.
   An over approximation is applied when the objective is to find a proof for a given goal."
  (c context)
  (g goal))

#|
The formula is split according to the following procedure that is applied
until a fixed-point:
Conjunctions are split into separate formulas.
Negations are distributed over disjunctions, resulting in separate formulas.
If the goal is \c false, adding new formulas is a no-op.
If the formula \c a is \c true, then nothing is added.
If the formula \c a is \c false, then the entire goal is replaced by the formula \c false.
|#
(defcfun "Z3_goal_assert" :void
  "Add a new formula `a` to the given goal."
  (c context)
  (g goal)
  (a ast))

(defcfun "Z3_goal_inconsistent" :bool
  "Return true if the given goal contains the formula false."
  (c context)
  (g goal))

(defcfun "Z3_goal_depth" :uint
  "Return the depth of the given goal. It tracks how many transformations were applied to it."
  (c context)
  (g goal))

(defcfun "Z3_goal_reset" :void
  "Erase all formulas from the given goal."
  (c context)
  (g goal))

(defcfun "Z3_goal_size" :uint
  "Return the number of formulas in the given goal."
  (c context)
  (g goal))

(defcfun "Z3_goal_formula" ast
  "Return a formula from the given goal."
  (c context)
  (g goal)
  (idx :uint)) ;; <= goal_size(c,g)

(defcfun "Z3_goal_num_exprs" :uint
  "Return the number of formulas, subformulas and terms in the given goal."
  (c context)
  (g goal))

(defcfun "Z3_goal_is_decided_sat" :bool
  "Return \c true if the goal is empty, and it is precise or the product of a under approximation"
  (c context)
  (g goal))

(defcfun "Z3_goal_is_decided_unsat" :bool
  "Return \c true if the goal contains false, and it is precise or the product of an over approximation."
  (c context)
  (g goal))

(defcfun "Z3_goal_translate" goal
  "Copy a goal `g` from the context `source` to the context `target`."
  (source context)
  (g goal)
  (target context))

(defcfun "Z3_goal_convert_model" model
  "Convert a model of the formulas of a goal to a model of an original goal.
   The model may be null, in which case the returned model is valid if the goal was
   established satisfiable."
  (c context)
  (g goal)
  (m model))

(defcfun "Z3_goal_to_string" :string
  "Convert a goal into a string."
  (c context)
  (g goal))

(defcfun "Z3_goal_to_dimacs_string" :string
  "Convert a goal into a DIMACS formatted string.
   The goal must be in CNF. You can convert a goal to CNF
   by applying the tseitin-cnf tactic. Bit-vectors are not automatically
   converted to Booleans either, so if the caller intends to
   preserve satisfiability, it should apply bit-blasting tactics.
   Quantifiers and theory atoms will not be encoded."
  (c context)
  (g goal))

;; Tactics and Probes

;; ...

;; Solvers

#|
 If the solver is used in a non incremental way (i.e. no calls to
#Z3_solver_push() or #Z3_solver_pop(), and no calls to
#Z3_solver_assert() or #Z3_solver_assert_and_track() after checking
satisfiability without an intervening #Z3_solver_reset()) then solver1
will be used. This solver will apply Z3's "default" tactic.

The "default" tactic will attempt to probe the logic used by the
assertions and will apply a specialized tactic if one is supported.
Otherwise the general `(and-then simplify smt)` tactic will be used.

If the solver is used in an incremental way then the combined solver
will switch to using solver2 (which behaves similarly to the general
"smt" tactic).

Note however it is possible to set the `solver2_timeout`,
`solver2_unknown`, and `ignore_solver1` parameters of the combined
solver to change its behaviour.

The function #Z3_solver_get_model retrieves a model if the
assertions is satisfiable (i.e., the result is \c
Z3_L_TRUE) and model construction is enabled.
The function #Z3_solver_get_model can also be used even
if the result is \c Z3_L_UNDEF, but the returned model
is not guaranteed to satisfy quantified assertions.

\remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc.
|#
(defcfun "Z3_mk_solver" solver
  "Create a new solver. This solver is a \"combined solver\" (see
   combined_solver module) that internally uses a non-incremental (solver1) and an
   incremental solver (solver2). This combined solver changes its behaviour based
   on how it is used and how its parameters are set."
  (c context))

#|
 Unlike #Z3_mk_solver() this solver
- Does not attempt to apply any logic specific tactics.
- Does not change its behaviour based on whether it used
incrementally/non-incrementally.

Note that these differences can result in very different performance
compared to #Z3_mk_solver().

The function #Z3_solver_get_model retrieves a model if the
assertions is satisfiable (i.e., the result is \c
Z3_L_TRUE) and model construction is enabled.
The function #Z3_solver_get_model can also be used even
if the result is \c Z3_L_UNDEF, but the returned model
is not guaranteed to satisfy quantified assertions.

\remark User must use #Z3_solver_inc_ref and #Z3_solver_dec_ref to manage solver objects.
Even if the context was created using #Z3_mk_context instead of #Z3_mk_context_rc.
|#
(defcfun "Z3_mk_simple_solver" solver
  "Create a new incremental solver.
   This is equivalent to applying the \"smt\" tactic."
  (c context))


(defcfun "Z3_mk_solver_for_logic" solver
  "Create a new solver customized for the given logic.
   It behaves like #Z3_mk_solver if the logic is unknown or unsupported."
  (c context)
  (logic sym))

(defcfun "Z3_mk_solver_from_tactic" solver
  "Create a new solver that is implemented using the given tactic.
   The solver supports the commands #Z3_solver_push and #Z3_solver_pop, but it
   will always solve each #Z3_solver_check from scratch."
  (c context)
  (tac tactic))

(defcfun "Z3_solver_translate" solver
  "Copy a solver `s` from the context `source` to the context `target`."
  (source context)
  (s solver)
  (target context))

#|
  This method is used for scenarios where \c src has been used to solve a set
of formulas and was interrupted. The \c dst solver may be a strengthening of \c src
obtained from cubing (assigning a subset of literals or adding constraints over the 
assertions available in \c src). If \c dst ends up being satisfiable, the model for \c dst
may not correspond to a model of the original formula due to inprocessing in \c src.
This method is used to take the side-effect of inprocessing into account when returning
a model for \c dst.
|#
(defcfun "Z3_solver_import_model_converter" :void
  "Ad-hoc method for importing model conversion from solver."
  (ctx context)
  (src solver)
  (dst solver))

(defcfun "Z3_solver_get_help" :string
  "Return a string describing all solver available parameters."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_param_descrs" param-descrs
  "Return the parameter description set for the given solver object."
  (c context)
  (s solver))

(defcfun "Z3_solver_set_params" :void
  "Set the given solver using the given parameters."
  (c context)
  (s solver)
  (p params))

(defcfun "Z3_solver_inc_ref" :void
  "Increment the reference counter of the given solver."
  (c context)
  (s solver))

(defcfun "Z3_solver_dec_ref" :void
  "Decrement the reference counter of the given solver."
  (c context)
  (s solver))

#|
Normally you should use Z3_interrupt to cancel solvers because only
one solver is enabled concurrently per context.
However, per GitHub issue #1006, there are use cases where
it is more convenient to cancel a specific solver. Solvers 
that are not selected for interrupts are left alone.
|#
(defcfun "Z3_solver_interrupt" :void
  "Solver local interrupt."
  (c context)
  (s solver))

(defcfun "Z3_solver_push" :void
  "Create a backtracking point.
   The solver contains a stack of assertions."
  (c context)
  (s solver))

(defcfun "Z3_solver_pop" :void
  "Backtrack `n` backtracking points."
  (c context)
  (s solver)
  (n :uint)) ;; n <= Z3_solver_get_num_scopes(c, s)

(defcfun "Z3_solver_reset" :void
  "Remove all assertions from the solver."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_num_scopes" :uint
  "Return the number of backtracking points."
  (c context)
  (s solver))

(defcfun "Z3_solver_assert" :void
  "Assert a constraint into the solver."
  #|
  The functions #Z3_solver_check and #Z3_solver_check_assumptions should be
  used to check whether the logical context is consistent or not.
  |#
  (c context)
  (s solver)
  (a ast))

(defcfun "Z3_solver_assert_and_track" :void
  "Assert a constraint `a` into the solver, and track it (in the unsat) core using
   the Boolean constant `p`."
  #|
  This API is an alternative to #Z3_solver_check_assumptions for extracting unsat cores.
  Both APIs can be used in the same solver. The unsat core will contain a combination
  of the Boolean variables provided using Z3_solver_assert_and_track and the Boolean literals
  provided using #Z3_solver_check_assumptions.
  |#
  (c context)
  (s solver)
  (a ast) ;; must be a Boolean expression
  (p ast)) ;; must be a Boolean constant (aka variable).

(defcfun "Z3_solver_from_file" :void
  "Load solver assertions from a file."
  (c context)
  (s solver)
  (file_name :string))

(defcfun "Z3_solver_from_string" :void
  "Load solver assertions from a string."
  (c context)
  (s solver)
  (str :string))

(defcfun "Z3_solver_get_assertions" ast-vector
  "Return the set of asserted formulas on the solver."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_units" ast-vector
  "Return the set of units modulo model conversion."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_trail" ast-vector
  "Return the trail modulo model conversion, in order of decision level
   The decision level can be retrieved using \c Z3_solver_get_level based on the trail."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_non_units" ast-vector
  "Return the set of non units in the solver state."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_levels" :void
  "Retrieve the decision depth of Boolean literals (variables or their negations).
   Assumes a check-sat call and no other calls (to extract models) have been invoked."
  (c context)
  (s solver)
  (literals ast-vector)
  (sz :uint)
  (levels :pointer)) ;; unsigned[] with size >= sz

(defcfun "Z3_solver_check" lbool
  "Check whether the assertions in a given solver are consistent or not."
  #|
  The function #Z3_solver_get_model retrieves a model if the
  assertions is satisfiable (i.e., the result is \c
  Z3_L_TRUE) and model construction is enabled.
  Note that if the call returns \c Z3_L_UNDEF, Z3 does not
  ensure that calls to #Z3_solver_get_model succeed and any models
  produced in this case are not guaranteed to satisfy the assertions.

  The function #Z3_solver_get_proof retrieves a proof if proof
  generation was enabled when the context was created, and the
  assertions are unsatisfiable (i.e., the result is \c Z3_L_FALSE).
  |#
  (c context)
  (s solver))

(defcfun "Z3_solver_check_assumptions" lbool
  "Check whether the assertions in the given solver and
   optional assumptions are consistent or not."
  #|
  The function #Z3_solver_get_unsat_core retrieves the subset of the
  assumptions used in the unsatisfiability proof produced by Z3.
  |#
  (c context)
  (s solver)
  (num_assumptions :uint)
  (assumptions :pointer)) ;; const ast[]

(defcfun "Z3_get_implied_equalities" lbool
  "Retrieve congruence class representatives for terms."
  #|
  The function can be used for relying on Z3 to identify equal terms under the current
  set of assumptions. The array of terms and array of class identifiers should have
  the same length. The class identifiers are numerals that are assigned to the same
  value for their corresponding terms if the current context forces the terms to be
  equal. You cannot deduce that terms corresponding to different numerals must be all different,
  (especially when using non-convex theories).
  All implied equalities are returned by this call.
  This means that two terms map to the same class identifier if and only if
  the current context implies that they are equal.

  A side-effect of the function is a satisfiability check on the assertions on the solver that is passed in.
  The function return \c Z3_L_FALSE if the current assertions are not satisfiable.
  |#
  (c context)
  (s solver)
  (num_terms :uint)
  (terms :pointer) ;; const ast[]
  (class_ids :pointer)) ;; output param unsigned[] size >= num_terms

(defcfun "Z3_solver_get_consequences" lbool
  "Retrieve consequences from solver that determine values of the supplied function symbols."
  (c context)
  (s solver)
  (assumptions ast-vector)
  (variables ast-vector)
  (consequences ast-vector)) ;; provide a new ast_vector. It will be modified during execution.

(defcfun "Z3_solver_cube" ast-vector
  "Extract a next cube for a solver. The last cube is the constant true or false.
   The number of (non-constant) cubes is by default 1. For the sat solver cubing is controlled
   using parameters sat.lookahead.cube.cutoff and sat.lookahead.cube.fraction."
  #|
  The third argument is a vector of variables that may be used for cubing.
  The contents of the vector is only used in the first call. The initial list of variables
  is used in subsequent calls until it returns the unsatisfiable cube. 
  The vector is modified to contain a set of Autarky variables that occur in clauses that
  are affected by the (last literal in the) cube. These variables could be used by a different
  cuber (on a different solver object) for further recursive cubing. 

  The last argument is a backtracking level. It instructs the cube process to backtrack below
  the indicated level for the next cube.
  |#
  (c context)
  (s solver)
  (vars ast-vector)
  (backtrack_level :uint))

(defcfun "Z3_solver_get_model" model
  "Retrieve the model for the last #Z3_solver_check or #Z3_solver_check_assumptions
   The error handler is invoked if a model is not available because
   the commands above were not invoked for the given solver, or if the result was Z3_L_FALSE."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_proof" ast
  "Retrieve the proof for the last #Z3_solver_check or #Z3_solver_check_assumptions
   The error handler is invoked if proof generation is not enabled,
   or if the commands above were not invoked for the given solver,
   or if the result was different from Z3_L_FALSE."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_unsat_core" ast-vector
  "Retrieve the unsat core for the last #Z3_solver_check_assumptions
   The unsat core is a subset of the assumptions `a`."
  #|
  By default, the unsat core will not be minimized. Generation of a minimized
  unsat core can be enabled via the `"sat.core.minimize"` and `"smt.core.minimize"`
  settings for SAT and SMT cores respectively. Generation of minimized unsat cores
  will be more expensive.
  |#
  (c context)
  (s solver))

(defcfun "Z3_solver_get_reason_unknown" :string
  "Return a brief justification for an \"unknown\" result (i.e., `Z3_L_UNDEF`) for
   the commands #Z3_solver_check and #Z3_solver_check_assumptions."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_statistics" stats
  "Return statistics for the given solver.
   User must use #Z3_stats_inc_ref and #Z3_stats_dec_ref to manage Z3_stats objects."
  (c context)
  (s solver))

(defcfun "Z3_solver_to_string" :string
  "Convert a solver into a string."
  (c context)
  (s solver))

(defcfun "Z3_solver_to_dimacs_string" :string
  "Convert a solver into a DIMACS formatted string.
   See Z3_goal_to_diamcs_string for requirements."
  (c context)
  (s solver)
  (include_names :bool))

;; Statistics

(defcfun "Z3_stats_to_string" :string
  "Convert a statistics into a string."
  (c context)
  (s stats))

(defcfun "Z3_stats_inc_ref" :string
  "Increment the reference counter of the given statistics object."
  (c context)
  (s stats))

(defcfun "Z3_stats_dec_ref" :string
  "Decrement the reference counter of the given statistics object."
  (c context)
  (s stats))

;; ...
