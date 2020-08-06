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

;; ...

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

;; 
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

;; ...

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
  "Add a new formula \c a to the given goal."
  (c context)
  (g goal)
  (a ast))

(defcfun "Z3_goal_inconsistent" :bool
  "Return \c true if the given goal contains the formula \c false."
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
  "Copy a goal \c g from the context \c source to the context \c target."
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
  "Assert a constraint \c a into the solver, and track it (in the unsat) core using
   the Boolean constant \c p."
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
  "Extract a next cube for a solver. The last cube is the constant \c true or \c false.
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
   the commands above were not invoked for the given solver, or if the result was \c Z3_L_FALSE."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_proof" ast
  "Retrieve the proof for the last #Z3_solver_check or #Z3_solver_check_assumptions
   The error handler is invoked if proof generation is not enabled,
   or if the commands above were not invoked for the given solver,
   or if the result was different from \c Z3_L_FALSE."
  (c context)
  (s solver))

(defcfun "Z3_solver_get_unsat_core" ast-vector
  "Retrieve the unsat core for the last #Z3_solver_check_assumptions
   The unsat core is a subset of the assumptions \c a."
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

