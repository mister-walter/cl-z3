(defpackage #:z3-c-types
  (:use #:cffi)
  (:shadow #:sort #:optimize)
  (:export
   #:Z3_config
   #:Z3_context
   #:Z3_symbol
   #:Z3_ast
   #:Z3_sort
   #:Z3_func_decl
   #:Z3_app
   #:Z3_pattern
   #:Z3_constructor
   #:Z3_constructor_list
   #:Z3_params
   #:Z3_param_descrs
   #:Z3_model
   #:Z3_func_interp
   #:Z3_func_entry
   #:Z3_fixedpoint
   #:Z3_optimize
   #:Z3_ast_vector
   #:Z3_ast_map
   #:Z3_goal
   #:Z3_tactic
   #:Z3_probe
   #:Z3_apply_results
   #:Z3_solver
   #:Z3_stats
   ;; Enums
   #:lbool
   #:symbol_kind
   #:parameter_kind
   #:sort_kind
   #:ast_kind
   #:decl_kind
   #:param_kind
   #:ast_print_mode
   #:error_code
   #:goal_prec
   ;; Foreign types
   #:config
   #:context
   #:sym
   #:ast
   #:sort
   #:func-decl
   #:app
   #:pattern
   #:constructor
   #:constructor-list
   #:params
   #:param-descrs
   #:model
   #:func-interp
   #:func-entry
   #:fixedpoint
   #:optimize
   #:ast-vector
   #:ast-map
   #:goal
   #:tactic
   #:probe
   #:apply-results
   #:solver
   #:stats
   ))

(defpackage #:z3-c
  (:shadowing-import-from #:z3-c-types #:sort #:optimize)
  (:use #:cl #:cffi #:z3-c-types)
  (:export
   #:Z3-GLOBAL-PARAM-SET
#:Z3-GLOBAL-PARAM-RESET-ALL
#:Z3-GLOBAL-PARAM-GET
#:Z3-MK-CONFIG
#:Z3-DEL-CONFIG
#:Z3-SET-PARAM-VALUE
#:Z3-MK-CONTEXT
#:Z3-DEL-CONTEXT
#:Z3-UPDATE-PARAM-VALUE
#:Z3-INTERRUPT
#:Z3-MK-PARAMS
#:Z3-PARAMS-INC-REF
#:Z3-PARAMS-DEC-REF
#:Z3-PARAMS-SET-BOOL
#:Z3-PARAMS-SET-UINT
#:Z3-PARAMS-SET-DOUBLE
#:Z3-PARAMS-SET-SYMBOL
#:Z3-PARAMS-TO-STRING
#:Z3-PARAMS-VALIDATE
#:Z3-PARAM-DESCRS-GET-KIND
#:Z3-PARAM-DESCRS-SIZE
#:Z3-PARAM-DESCRS-GET-NAME
#:Z3-PARAM-DESCRS-GET-DOCUMENTATION
#:Z3-PARAM-DESCRS-TO-STRING
#:Z3-MK-INT-SYMBOL
#:Z3-MK-STRING-SYMBOL
#:Z3-MK-UNINTERPRETED-SORT
#:Z3-MK-BOOL-SORT
#:Z3-MK-INT-SORT
#:Z3-MK-REAL-SORT
#:Z3-MK-BV-SORT
#:Z3-MK-FINITE-DOMAIN-SORT
#:Z3-MK-ARRAY-SORT
#:Z3-MK-ARRAY-SORT-N
#:Z3-MK-TUPLE-SORT
#:Z3-MK-ENUMERATION-SORT
#:Z3-MK-LIST-SORT
#:Z3-MK-CONSTRUCTOR
#:Z3-DEL-CONSTRUCTOR
#:Z3-MK-DATATYPE
#:Z3-MK-CONSTRUCTOR-LIST
#:Z3-DEL-CONSTRUCTOR-LIST
#:Z3-MK-DATATYPES
#:Z3-QUERY-CONSTRUCTOR
#:Z3-MK-FUNC-DECL
#:Z3-MK-APP
#:Z3-MK-CONST
#:Z3-MK-FRESH-FUNC-DECL
#:Z3-MK-FRESH-CONST
#:Z3-ADD-REC-DEF
#:Z3-MK-TRUE
#:Z3-MK-FALSE
#:Z3-MK-EQ
#:Z3-MK-DISTINCT
#:Z3-MK-NOT
#:Z3-MK-ITE
#:Z3-MK-IFF
#:Z3-MK-IMPLIES
#:Z3-MK-XOR
#:Z3-MK-AND
#:Z3-MK-OR
#:Z3-MK-ADD
#:Z3-MK-MUL
#:Z3-MK-SUB
#:Z3-MK-UNARY-MINUS
#:Z3-MK-DIV
#:Z3-MK-MOD
#:Z3-MK-REM
#:Z3-MK-POWER
#:Z3-MK-LT
#:Z3-MK-LE
#:Z3-MK-GT
#:Z3-MK-GE
#:Z3-MK-DIVIDES
#:Z3-MK-INT2REAL
#:Z3-MK-REAL2INT
#:Z3-MK-IS-INT
#:Z3-MK-BVNOT
#:Z3-MK-BVREDAND
#:Z3-MK-BVREDOR
#:Z3-MK-BVAND
#:Z3-MK-BVOR
#:Z3-MK-BVXOR
#:Z3-MK-BVNAND
#:Z3-MK-BVNOR
#:Z3-MK-BVXNOR
#:Z3-MK-BVNEG
#:Z3-MK-BVADD
#:Z3-MK-BVSUB
#:Z3-MK-BVMUL
#:Z3-MK-BVUDIV
#:Z3-MK-BVSDIV
#:Z3-MK-BVUREM
#:Z3-MK-BVSMOD
#:Z3-MK-BVULT
#:Z3-MK-BVSLT
#:Z3-MK-BVULE
#:Z3-MK-BVSLE
#:Z3-MK-BVUGE
#:Z3-MK-BVSGE
#:Z3-MK-BVUGT
#:Z3-MK-BVSGT
#:Z3-MK-CONCAT
#:Z3-MK-EXTRACT
#:Z3-MK-SIGN-EXT
#:Z3-MK-ZERO-EXT
#:Z3-MK-REPEAT
#:Z3-MK-BVSHL
#:Z3-MK-BVLSHR
#:Z3-MK-BVASHR
#:Z3-MK-ROTATE-LEFT
#:Z3-MK-ROTATE-RIGHT
#:Z3-MK-EXT-ROTATE-LEFT
#:Z3-MK-EXT-ROTATE-RIGHT
#:Z3-MK-INT2BV
#:Z3-MK-BV2INT
#:Z3-MK-BVADD-NO-OVERFLOW
#:Z3-MK-BVADD-NO-UNDERFLOW
#:Z3-MK-BVSUB-NO-OVERFLOW
#:Z3-MK-BVSUB-NO-UNDERFLOW
#:Z3-MK-BVSDIV-NO-OVERFLOW
#:Z3-MK-BVNEG-NO-OVERFLOW
#:Z3-MK-BVMUL-NO-OVERFLOW
#:Z3-MK-BVMUL-NO-UNDERFLOW
#:Z3-MK-SELECT
#:Z3-MK-SELECT-N
#:Z3-MK-STORE
#:Z3-MK-STORE-N
#:Z3-MK-CONST-ARRAY
#:Z3-MK-MAP
#:Z3-MK-ARRAY-DEFAULT
#:Z3-MK-AS-ARRAY
#:Z3-MK-SET-HAS-SIZE
#:Z3-MK-SET-SORT
#:Z3-MK-EMPTY-SET
#:Z3-MK-FULL-SET
#:Z3-MK-SET-ADD
#:Z3-MK-SET-DEL
#:Z3-MK-SET-UNION
#:Z3-MK-SET-INTERSECT
#:Z3-MK-SET-DIFFERENCE
#:Z3-MK-SET-COMPLEMENT
#:Z3-MK-SET-MEMBER
#:Z3-MK-SET-SUBSET
#:Z3-MK-ARRAY-EXT
#:Z3-MK-NUMERAL
#:Z3-MK-REAL
#:Z3-MK-INT
#:Z3-MK-UNSIGNED-INT
#:Z3-MK-INT64
#:Z3-MK-UNSIGNED-INT64
#:Z3-MK-BV-NUMERAL
#:Z3-MK-SEQ-SORT
#:Z3-IS-SEQ-SORT
#:Z3-GET-SEQ-SORT-BASIS
#:Z3-MK-RE-SORT
#:Z3-IS-RE-SORT
#:Z3-GET-RE-SORT-BASIS
#:Z3-MK-STRING-SORT
#:Z3-IS-STRING-SORT
#:Z3-MK-STRING
#:Z3-MK-LSTRING
#:Z3-IS-STRING
#:Z3-GET-STRING
#:Z3-GET-LSTRING
#:Z3-MK-SEQ-EMPTY
#:Z3-MK-SEQ-UNIT
#:Z3-MK-SEQ-CONCAT
#:Z3-MK-SEQ-PREFIX
#:Z3-MK-SEQ-SUFFIX
#:Z3-MK-SEQ-CONTAINS
#:Z3-MK-STR-LT
#:Z3-MK-STR-LE
#:Z3-MK-SEQ-EXTRACT
#:Z3-MK-SEQ-REPLACE
#:Z3-MK-SEQ-AT
#:Z3-MK-SEQ-NTH
#:Z3-MK-SEQ-LENGTH
#:Z3-MK-SEQ-INDEX
#:Z3-MK-SEQ-LAST-INDEX
#:Z3-MK-STR-TO-INT
#:Z3-MK-INT-TO-STR
#:Z3-MK-LINEAR-ORDER
#:Z3-MK-PARTIAL-ORDER
#:Z3-MK-PIECEWISE-LINEAR-ORDER
#:Z3-MK-TREE-ORDER
#:Z3-MK-TRANSITIVE-CLOSURE
#:Z3-GET-SYMBOL-KIND
#:Z3-GET-SYMBOL-INT
#:Z3-GET-SYMBOL-STRING
#:Z3-GET-SORT-NAME
#:Z3-GET-SORT-ID
#:Z3-SORT-TO-AST
#:Z3-IS-EQ-SORT
#:Z3-GET-SORT-KIND
#:Z3-GET-BV-SORT-SIZE
#:Z3-GET-FINITE-DOMAIN-SORT-SIZE
#:Z3-GET-ARRAY-SORT-DOMAIN
#:Z3-GET-ARRAY-SORT-RANGE
#:Z3-GET-TUPLE-SORT-MK-DECL
#:Z3-GET-TUPLE-SORT-NUM-FIELDS
#:Z3-GET-TUPLE-SORT-FIELD-DECL
#:Z3-GET-DATATYPE-SORT-NUM-CONSTRUCTORS
#:Z3-GET-DATATYPE-SORT-CONSTRUCTOR
#:Z3-GET-DATATYPE-SORT-RECOGNIZER
#:Z3-GET-DATATYPE-SORT-CONSTRUCTOR-ACCESSOR
#:Z3-GET-DATATYPE-UPDATE-FIELD
#:Z3-MK-ATMOST
#:Z3-MK-ATLEAST
#:Z3-MK-PBLE
#:Z3-MK-PBGE
#:Z3-MK-PBEQ
#:Z3-GET-DECL-NAME
#:Z3-GET-DECL-KIND
#:Z3-GET-SORT
#:Z3-IS-WELL-SORTED
#:Z3-GET-BOOL-VALUE
#:Z3-GET-AST-KIND
#:Z3-IS-APP
#:Z3-IS-NUMERAL-AST
#:Z3-IS-ALGEBRAIC-NUMBER
#:Z3-TO-APP
#:Z3-TO-FUNC-DECL
#:Z3-GET-NUMERAL-STRING
#:Z3-GET-NUMERAL-DECIMAL-STRING
#:Z3-GET-NUMERAL-DOUBLE
#:Z3-GET-NUMERATOR
#:Z3-GET-DENOMINATOR
#:Z3-GET-NUMERAL-SMALL
#:Z3-GET-NUMERAL-INT
#:Z3-GET-NUMERAL-UINT
#:Z3-GET-NUMERAL-UINT64
#:Z3-GET-NUMERAL-INT64
#:Z3-GET-NUMERAL-RATIONAL-INT64
#:Z3-APP-TO-AST
#:Z3-GET-APP-DECL
#:Z3-GET-APP-NUM-ARGS
#:Z3-GET-APP-ARG
#:Z3-MK-MODEL
#:Z3-MODEL-INC-REF
#:Z3-MODEL-DEC-REF
#:Z3-MODEL-EVAL
#:Z3-MODEL-GET-CONST-INTERP
#:Z3-MODEL-HAS-INTERP
#:Z3-MODEL-GET-FUNC-INTERP
#:Z3-MODEL-GET-NUM-CONSTS
#:Z3-MODEL-GET-CONST-DECL
#:Z3-MODEL-GET-NUM-FUNCS
#:Z3-MODEL-GET-FUNC-DECL
#:Z3-MODEL-GET-NUM-SORTS
#:Z3-MODEL-GET-SORT
#:Z3-MODEL-GET-SORT-UNIVERSE
#:Z3-MODEL-TRANSLATE
#:Z3-IS-AS-ARRAY
#:Z3-GET-AS-ARRAY-FUNC-DECL
#:Z3-ADD-FUNC-INTERP
#:Z3-ADD-CONST-INTERP
#:Z3-FUNC-INTERP-INC-REF
#:Z3-FUNC-INTERP-DEC-REF
#:Z3-FUNC-INTERP-GET-NUM-ENTRIES
#:Z3-FUNC-INTERP-GET-ENTRY
#:Z3-FUNC-INTERP-GET-ELSE
#:Z3-FUNC-INTERP-SET-ELSE
#:Z3-FUNC-INTERP-GET-ARITY
#:Z3-FUNC-INTERP-ADD-ENTRY
#:Z3-FUNC-ENTRY-INC-REF
#:Z3-FUNC-ENTRY-DEC-REF
#:Z3-FUNC-ENTRY-GET-VALUE
#:Z3-FUNC-ENTRY-GET-NUM-ARGS
#:Z3-FUNC-ENTRY-GET-ARG
#:Z3-OPEN-LOG
#:Z3-APPEND-LOG
#:Z3-CLOSE-LOG
#:Z3-TOGGLE-WARNING-MESSAGES
#:Z3-SET-AST-PRINT-MODE
#:Z3-AST-TO-STRING
#:Z3-PATTERN-TO-STRING
#:Z3-SORT-TO-STRING
#:Z3-FUNC-DECL-TO-STRING
#:Z3-MODEL-TO-STRING
#:Z3-BENCHMARK-TO-SMTLIB-STRING
#:Z3-PARSE-SMTLIB2-STRING
#:Z3-PARSE-SMTLIB2-FILE
#:Z3-EVAL-SMTLIB2-STRING
#:Z3-GET-ERROR-CODE
#:Z3-SET-ERROR-HANDLER
#:Z3-SET-ERROR
#:Z3-GET-ERROR-MSG
#:Z3-MK-GOAL
#:Z3-GOAL-INC-REF
#:Z3-GOAL-DEC-REF
#:Z3-GOAL-PRECISION
#:Z3-GOAL-ASSERT
#:Z3-GOAL-INCONSISTENT
#:Z3-GOAL-DEPTH
#:Z3-GOAL-RESET
#:Z3-GOAL-SIZE
#:Z3-GOAL-FORMULA
#:Z3-GOAL-NUM-EXPRS
#:Z3-GOAL-IS-DECIDED-SAT
#:Z3-GOAL-IS-DECIDED-UNSAT
#:Z3-GOAL-TRANSLATE
#:Z3-GOAL-CONVERT-MODEL
#:Z3-GOAL-TO-STRING
#:Z3-GOAL-TO-DIMACS-STRING
#:Z3-MK-SOLVER
#:Z3-MK-SIMPLE-SOLVER
#:Z3-MK-SOLVER-FOR-LOGIC
#:Z3-MK-SOLVER-FROM-TACTIC
#:Z3-SOLVER-TRANSLATE
#:Z3-SOLVER-IMPORT-MODEL-CONVERTER
#:Z3-SOLVER-GET-HELP
#:Z3-SOLVER-GET-PARAM-DESCRS
#:Z3-SOLVER-SET-PARAMS
#:Z3-SOLVER-INC-REF
#:Z3-SOLVER-DEC-REF
#:Z3-SOLVER-INTERRUPT
#:Z3-SOLVER-PUSH
#:Z3-SOLVER-POP
#:Z3-SOLVER-RESET
#:Z3-SOLVER-GET-NUM-SCOPES
#:Z3-SOLVER-ASSERT
#:Z3-SOLVER-ASSERT-AND-TRACK
#:Z3-SOLVER-FROM-FILE
#:Z3-SOLVER-FROM-STRING
#:Z3-SOLVER-GET-ASSERTIONS
#:Z3-SOLVER-GET-UNITS
#:Z3-SOLVER-GET-TRAIL
#:Z3-SOLVER-GET-NON-UNITS
#:Z3-SOLVER-GET-LEVELS
#:Z3-SOLVER-CHECK
#:Z3-SOLVER-CHECK-ASSUMPTIONS
#:Z3-GET-IMPLIED-EQUALITIES
#:Z3-SOLVER-GET-CONSEQUENCES
#:Z3-SOLVER-CUBE
#:Z3-SOLVER-GET-MODEL
#:Z3-SOLVER-GET-PROOF
#:Z3-SOLVER-GET-UNSAT-CORE
#:Z3-SOLVER-GET-REASON-UNKNOWN
#:Z3-SOLVER-GET-STATISTICS
#:Z3-SOLVER-TO-STRING
#:Z3-SOLVER-TO-DIMACS-STRING
#:Z3-STATS-TO-STRING
#:Z3-STATS-INC-REF
#:Z3-STATS-DEC-REF))
