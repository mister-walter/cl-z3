(defpackage #:z3-c-types
  (:use #:cffi)
  (:shadow #:sort)
  (:export
   ;;#:Z3_bool
   #:Z3_char_ptr
   #:Z3_string_ptr
   #:Z3_config
   #:Z3_context
   #:Z3_params
   #:Z3_param_descrs
   #:Z3_constructor
   #:Z3_constructor_list
   #:Z3_symbol
   #:Z3_func_decl
   #:Z3_ast
   #:Z3_ast_vector
   #:Z3_sort
   #:Z3_pattern
   #:Z3_model
   #:Z3_stats
   #:Z3_goal
   #:Z3_solver
   #:Z3_tactic
   ;; Enums
   #:lbool
   #:symbol_kind
   #:parameter_kind
   #:sort_kind
   #:decl_kind
   #:param_kind
   #:ast_print_mode
   #:error_code
   #:goal_prec
   ;; Foreign types
   #:sym
   #:config
   #:context
   #:params
   #:param-descrs
   #:func-decl
   #:sort
   #:constructor
   #:constructor-list
   #:ast
   #:ast-vector
   #:solver
   #:goal
   #:model
   #:tactic
   #:stats))

(defpackage #:z3-c
  (:shadowing-import-from #:z3-c-types #:sort)
  (:use #:cl #:cffi #:z3-c-types))
