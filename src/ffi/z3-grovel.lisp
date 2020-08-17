
#|
(pushnew (truename "/home/drew/lisp-z3/") ql:*local-project-directories* )
(ql:register-local-projects)
(ql:quickload :lisp-z3)

You may need to 

1) update your CPATH environment variable to include the directory
where z3.h is included and

2) update your LD_LIBRARY_PATH environment variable to include the
directory where your libz3.dylib or libz3.so file is i

For example in my .bashrc file I have:

export CPATH=$HOME/bin/include
export LD_LIBRARY_PATH=$HOME/bin/lib

|#
(in-package :z3-c-types)

; Better to use environment variables; see above.
; (cc-flags "-I /Users/pete/bin/include/")
(include "z3.h")

(feature z3-safe-errors "SAFE_ERRORS")

;; Most of the types in the C API are opaque pointers.
(ctype Z3_config "Z3_config")
(ctype Z3_context "Z3_context")
(ctype Z3_symbol "Z3_symbol")
(ctype Z3_ast "Z3_ast")
(ctype Z3_sort "Z3_sort")
(ctype Z3_func_decl "Z3_func_decl")
(ctype Z3_app "Z3_app")
(ctype Z3_pattern "Z3_pattern")
(ctype Z3_constructor "Z3_constructor")
(ctype Z3_constructor_list "Z3_constructor_list")
(ctype Z3_params "Z3_params")
(ctype Z3_param_descrs "Z3_param_descrs")
(ctype Z3_model "Z3_model")
(ctype Z3_func_interp "Z3_func_interp")
(ctype Z3_func_entry "Z3_func_entry")
(ctype Z3_fixedpoint "Z3_fixedpoint")
(ctype Z3_optimize "Z3_optimize")
(ctype Z3_ast_vector "Z3_ast_vector")
(ctype Z3_ast_map "Z3_ast_map")
(ctype Z3_goal "Z3_goal")
(ctype Z3_tactic "Z3_tactic")
(ctype Z3_probe "Z3_probe")
(ctype Z3_apply_result "Z3_apply_result")
(ctype Z3_solver "Z3_solver")
(ctype Z3_stats "Z3_stats")

;; Lifted Boolean type
(cenum (lbool)
       ((:L_FALSE "Z3_L_FALSE"))
       ((:L_UNDEF "Z3_L_UNDEF"))
       ((:L_TRUE "Z3_L_TRUE")))

;; The different kinds of symbol
(cenum (symbol_kind)
       ((:INT_SYMBOL "Z3_INT_SYMBOL"))
       ((:STRING_SYMBOL "Z3_STRING_SYMBOL")))

;; The different kinds of parameters that can be associated with function symbols.
(cenum (parameter_kind)
       ((:PARAMETER_INT "Z3_PARAMETER_INT"))
       ((:PARAMETER_DOUBLE "Z3_PARAMETER_DOUBLE"))
       ((:PARAMETER_RATIONAL "Z3_PARAMETER_RATIONAL"))
       ((:PARAMETER_SYMBOL "Z3_PARAMETER_SYMBOL"))
       ((:PARAMETER_SORT "Z3_PARAMETER_SORT"))
       ((:PARAMETER_AST "Z3_PARAMETER_AST"))
       ((:PARAMETER_FUNC_DECL "Z3_PARAMETER_FUNC_DECL")))

;; The different kinds of Z3 types
(cenum (sort_kind)
       ((:BOOL_SORT "Z3_BOOL_SORT"))
       ((:INT_SORT "Z3_INT_SORT"))
       ((:REAL_SORT "Z3_REAL_SORT"))
       ((:BV_SORT "Z3_BV_SORT"))
       ((:ARRAY_SORT "Z3_ARRAY_SORT"))
       ((:DATATYPE_SORT "Z3_DATATYPE_SORT"))
       ((:RELATION_SORT "Z3_RELATION_SORT"))
       ((:FINITE_DOMAIN_SORT "Z3_FINITE_DOMAIN_SORT"))
       ((:FLOATING_POINT_SORT "Z3_FLOATING_POINT_SORT"))
       ((:ROUNDING_MODE_SORT "Z3_ROUNDING_MODE_SORT"))
       ((:SEQ_SORT "Z3_SEQ_SORT"))
       ((:RE_SORT "Z3_RE_SORT"))
       ((:UNKNOWN_SORT "Z3_UNKNOWN_SORT")))

;; The different kinds of Z3 AST (abstract syntax trees). That is, terms, formulas and types.
(cenum (ast_kind)
       ((:NUMERAL_AST "Z3_NUMERAL_AST"))
       ((:APP_AST "Z3_APP_AST"))
       ((:VAR_AST "Z3_VAR_AST"))
       ((:QUANTIFIER_AST "Z3_QUANTIFIER_AST"))
       ((:SORT_AST "Z3_SORT_AST"))
       ((:FUNC_DECL_AST "Z3_FUNC_DECL_AST"))
       ((:UNKNOWN_AST "Z3_UNKNOWN_AST")))

;; The different kinds of interpreted function kinds.
(cenum (decl_kind)
      ;; Basic
       ((:OP_TRUE "Z3_OP_TRUE"))
       ((:OP_FALSE "Z3_OP_FALSE"))
       ((:OP_EQ "Z3_OP_EQ"))
       ((:OP_DISTINCT "Z3_OP_DISTINCT"))
       ((:OP_ITE "Z3_OP_ITE"))
       ((:OP_AND "Z3_OP_AND"))
       ((:OP_OR "Z3_OP_OR"))
       ((:OP_IFF "Z3_OP_IFF"))
       ((:OP_XOR "Z3_OP_XOR"))
       ((:OP_NOT "Z3_OP_NOT"))
       ((:OP_IMPLIES "Z3_OP_IMPLIES"))
       ((:OP_OEQ "Z3_OP_OEQ"))

       ;; Arithmetic
       ((:OP_ANUM "Z3_OP_ANUM"))
       ((:OP_AGNUM "Z3_OP_AGNUM"))
       ((:OP_LE "Z3_OP_LE"))
       ((:OP_GE "Z3_OP_GE"))
       ((:OP_LT "Z3_OP_LT"))
       ((:OP_GT "Z3_OP_GT"))
       ((:OP_ADD "Z3_OP_ADD"))
       ((:OP_SUB "Z3_OP_SUB"))
       ((:OP_UMINUS "Z3_OP_UMINUS"))
       ((:OP_MUL "Z3_OP_MUL"))
       ((:OP_DIV "Z3_OP_DIV"))
       ((:OP_IDIV "Z3_OP_IDIV"))
       ((:OP_REM "Z3_OP_REM"))
       ((:OP_MOD "Z3_OP_MOD"))
       ((:OP_TO_REAL "Z3_OP_TO_REAL"))
       ((:OP_TO_INT "Z3_OP_TO_INT"))
       ((:OP_IS_INT "Z3_OP_IS_INT"))
       ((:OP_POWER "Z3_OP_POWER"))

       ;; Arrays & Sets
       ((:OP_STORE "Z3_OP_STORE"))
       ((:OP_SELECT "Z3_OP_SELECT"))
       ((:OP_CONST_ARRAY "Z3_OP_CONST_ARRAY"))
       ((:OP_ARRAY_MAP "Z3_OP_ARRAY_MAP"))
       ((:OP_ARRAY_DEFAULT "Z3_OP_ARRAY_DEFAULT"))
       ((:OP_SET_UNION "Z3_OP_SET_UNION"))
       ((:OP_SET_INTERSECT "Z3_OP_SET_INTERSECT"))
       ((:OP_SET_DIFFERENCE "Z3_OP_SET_DIFFERENCE"))
       ((:OP_SET_COMPLEMENT "Z3_OP_SET_COMPLEMENT"))
       ((:OP_SET_SUBSET "Z3_OP_SET_SUBSET"))
       ((:OP_AS_ARRAY "Z3_OP_AS_ARRAY"))
       ((:OP_ARRAY_EXT "Z3_OP_ARRAY_EXT"))
       ((:OP_SET_HAS_SIZE "Z3_OP_SET_HAS_SIZE"))
       ((:OP_SET_CARD "Z3_OP_SET_CARD"))

       ;; Bit-vectors
       ((:OP_BNUM "Z3_OP_BNUM"))
       ((:OP_BIT1 "Z3_OP_BIT1"))
       ((:OP_BIT0 "Z3_OP_BIT0"))
       ((:OP_BNEG "Z3_OP_BNEG"))
       ((:OP_BADD "Z3_OP_BADD"))
       ((:OP_BSUB "Z3_OP_BSUB"))
       ((:OP_BMUL "Z3_OP_BMUL"))

       ((:OP_BSDIV "Z3_OP_BSDIV"))
       ((:OP_BUDIV "Z3_OP_BUDIV"))
       ((:OP_BSREM "Z3_OP_BSREM"))
       ((:OP_BUREM "Z3_OP_BUREM"))
       ((:OP_BSMOD "Z3_OP_BSMOD"))

       ;; special functions to record the division by 0 cases
       ;; these are internal functions
       ((:OP_BSDIV0 "Z3_OP_BSDIV0"))
       ((:OP_BUDIV0 "Z3_OP_BUDIV0"))
       ((:OP_BSREM0 "Z3_OP_BSREM0"))
       ((:OP_BUREM0 "Z3_OP_BUREM0"))
       ((:OP_BSMOD0 "Z3_OP_BSMOD0"))

       ((:OP_ULEQ "Z3_OP_ULEQ"))
       ((:OP_SLEQ "Z3_OP_SLEQ"))
       ((:OP_UGEQ "Z3_OP_UGEQ"))
       ((:OP_SGEQ "Z3_OP_SGEQ"))
       ((:OP_ULT "Z3_OP_ULT"))
       ((:OP_SLT "Z3_OP_SLT"))
       ((:OP_UGT "Z3_OP_UGT"))
       ((:OP_SGT "Z3_OP_SGT"))

       ((:OP_BAND "Z3_OP_BAND"))
       ((:OP_BOR "Z3_OP_BOR"))
       ((:OP_BNOT "Z3_OP_BNOT"))
       ((:OP_BXOR "Z3_OP_BXOR"))
       ((:OP_BNAND "Z3_OP_BNAND"))
       ((:OP_BNOR "Z3_OP_BNOR"))
       ((:OP_BXNOR "Z3_OP_BXNOR"))

       ((:OP_CONCAT "Z3_OP_CONCAT"))
       ((:OP_SIGN_EXT "Z3_OP_SIGN_EXT"))
       ((:OP_ZERO_EXT "Z3_OP_ZERO_EXT"))
       ((:OP_EXTRACT "Z3_OP_EXTRACT"))
       ((:OP_REPEAT "Z3_OP_REPEAT"))

       ((:OP_BREDOR "Z3_OP_BREDOR"))
       ((:OP_BREDAND "Z3_OP_BREDAND"))
       ((:OP_BCOMP "Z3_OP_BCOMP"))

       ((:OP_BSHL "Z3_OP_BSHL"))
       ((:OP_BLSHR "Z3_OP_BLSHR"))
       ((:OP_BASHR "Z3_OP_BASHR"))
       ((:OP_ROTATE_LEFT "Z3_OP_ROTATE_LEFT"))
       ((:OP_ROTATE_RIGHT "Z3_OP_ROTATE_RIGHT"))
       ((:OP_EXT_ROTATE_LEFT "Z3_OP_EXT_ROTATE_LEFT"))
       ((:OP_EXT_ROTATE_RIGHT "Z3_OP_EXT_ROTATE_RIGHT"))

       ((:OP_BIT2BOOL "Z3_OP_BIT2BOOL"))
       ((:OP_INT2BV "Z3_OP_INT2BV"))
       ((:OP_BV2INT "Z3_OP_BV2INT"))
       ((:OP_CARRY "Z3_OP_CARRY"))
       ((:OP_XOR3 "Z3_OP_XOR3"))

       ((:OP_BSMUL_NO_OVFL "Z3_OP_BSMUL_NO_OVFL"))
       ((:OP_BUMUL_NO_OVFL "Z3_OP_BUMUL_NO_OVFL"))
       ((:OP_BSMUL_NO_UDFL "Z3_OP_BSMUL_NO_UDFL"))
       ((:OP_BSDIV_I "Z3_OP_BSDIV_I"))
       ((:OP_BUDIV_I "Z3_OP_BUDIV_I"))
       ((:OP_BSREM_I "Z3_OP_BSREM_I"))
       ((:OP_BUREM_I "Z3_OP_BUREM_I"))
       ((:OP_BSMOD_I "Z3_OP_BSMOD_I"))

       ;; Proofs
       ((:OP_PR_UNDEF "Z3_OP_PR_UNDEF"))
       ((:OP_PR_TRUE "Z3_OP_PR_TRUE"))
       ((:OP_PR_ASSERTED "Z3_OP_PR_ASSERTED"))
       ((:OP_PR_GOAL "Z3_OP_PR_GOAL"))
       ((:OP_PR_MODUS_PONENS "Z3_OP_PR_MODUS_PONENS"))
       ((:OP_PR_REFLEXIVITY "Z3_OP_PR_REFLEXIVITY"))
       ((:OP_PR_SYMMETRY "Z3_OP_PR_SYMMETRY"))
       ((:OP_PR_TRANSITIVITY "Z3_OP_PR_TRANSITIVITY"))
       ((:OP_PR_TRANSITIVITY_STAR "Z3_OP_PR_TRANSITIVITY_STAR"))
       ((:OP_PR_MONOTONICITY "Z3_OP_PR_MONOTONICITY"))
       ((:OP_PR_QUANT_INTRO "Z3_OP_PR_QUANT_INTRO"))
       ((:OP_PR_BIND "Z3_OP_PR_BIND"))
       ((:OP_PR_DISTRIBUTIVITY "Z3_OP_PR_DISTRIBUTIVITY"))
       ((:OP_PR_AND_ELIM "Z3_OP_PR_AND_ELIM"))
       ((:OP_PR_NOT_OR_ELIM "Z3_OP_PR_NOT_OR_ELIM"))
       ((:OP_PR_REWRITE "Z3_OP_PR_REWRITE"))
       ((:OP_PR_REWRITE_STAR "Z3_OP_PR_REWRITE_STAR"))
       ((:OP_PR_PULL_QUANT "Z3_OP_PR_PULL_QUANT"))
       ((:OP_PR_PUSH_QUANT "Z3_OP_PR_PUSH_QUANT"))
       ((:OP_PR_ELIM_UNUSED_VARS "Z3_OP_PR_ELIM_UNUSED_VARS"))
       ((:OP_PR_DER "Z3_OP_PR_DER"))
       ((:OP_PR_QUANT_INST "Z3_OP_PR_QUANT_INST"))
       ((:OP_PR_HYPOTHESIS "Z3_OP_PR_HYPOTHESIS"))
       ((:OP_PR_LEMMA "Z3_OP_PR_LEMMA"))
       ((:OP_PR_UNIT_RESOLUTION "Z3_OP_PR_UNIT_RESOLUTION"))
       ((:OP_PR_IFF_TRUE "Z3_OP_PR_IFF_TRUE"))
       ((:OP_PR_IFF_FALSE "Z3_OP_PR_IFF_FALSE"))
       ((:OP_PR_COMMUTATIVITY "Z3_OP_PR_COMMUTATIVITY"))
       ((:OP_PR_DEF_AXIOM "Z3_OP_PR_DEF_AXIOM"))
       ((:OP_PR_ASSUMPTION_ADD "Z3_OP_PR_ASSUMPTION_ADD"))
       ((:OP_PR_LEMMA_ADD "Z3_OP_PR_LEMMA_ADD"))
       ((:OP_PR_REDUNDANT_DEL "Z3_OP_PR_REDUNDANT_DEL"))
       ((:OP_PR_CLAUSE_TRAIL "Z3_OP_PR_CLAUSE_TRAIL"))
       ((:OP_PR_DEF_INTRO "Z3_OP_PR_DEF_INTRO"))
       ((:OP_PR_APPLY_DEF "Z3_OP_PR_APPLY_DEF"))
       ((:OP_PR_IFF_OEQ "Z3_OP_PR_IFF_OEQ"))
       ((:OP_PR_NNF_POS "Z3_OP_PR_NNF_POS"))
       ((:OP_PR_NNF_NEG "Z3_OP_PR_NNF_NEG"))
       ((:OP_PR_SKOLEMIZE "Z3_OP_PR_SKOLEMIZE"))
       ((:OP_PR_MODUS_PONENS_OEQ "Z3_OP_PR_MODUS_PONENS_OEQ"))
       ((:OP_PR_TH_LEMMA "Z3_OP_PR_TH_LEMMA"))
       ((:OP_PR_HYPER_RESOLVE "Z3_OP_PR_HYPER_RESOLVE"))

       ;; Relational algebra
       ((:OP_RA_STORE "Z3_OP_RA_STORE"))
       ((:OP_RA_EMPTY "Z3_OP_RA_EMPTY"))
       ((:OP_RA_IS_EMPTY "Z3_OP_RA_IS_EMPTY"))
       ((:OP_RA_JOIN "Z3_OP_RA_JOIN"))
       ((:OP_RA_UNION "Z3_OP_RA_UNION"))
       ((:OP_RA_WIDEN "Z3_OP_RA_WIDEN"))
       ((:OP_RA_PROJECT "Z3_OP_RA_PROJECT"))
       ((:OP_RA_FILTER "Z3_OP_RA_FILTER"))
       ((:OP_RA_NEGATION_FILTER "Z3_OP_RA_NEGATION_FILTER"))
       ((:OP_RA_RENAME "Z3_OP_RA_RENAME"))
       ((:OP_RA_COMPLEMENT "Z3_OP_RA_COMPLEMENT"))
       ((:OP_RA_SELECT "Z3_OP_RA_SELECT"))
       ((:OP_RA_CLONE "Z3_OP_RA_CLONE"))
       ((:OP_FD_CONSTANT "Z3_OP_FD_CONSTANT"))
       ((:OP_FD_LT "Z3_OP_FD_LT"))

       ;; Sequences
       ((:OP_SEQ_UNIT "Z3_OP_SEQ_UNIT"))
       ((:OP_SEQ_EMPTY "Z3_OP_SEQ_EMPTY"))
       ((:OP_SEQ_CONCAT "Z3_OP_SEQ_CONCAT"))
       ((:OP_SEQ_PREFIX "Z3_OP_SEQ_PREFIX"))
       ((:OP_SEQ_SUFFIX "Z3_OP_SEQ_SUFFIX"))
       ((:OP_SEQ_CONTAINS "Z3_OP_SEQ_CONTAINS"))
       ((:OP_SEQ_EXTRACT "Z3_OP_SEQ_EXTRACT"))
       ((:OP_SEQ_REPLACE "Z3_OP_SEQ_REPLACE"))
       ((:OP_SEQ_AT "Z3_OP_SEQ_AT"))
       ((:OP_SEQ_NTH "Z3_OP_SEQ_NTH"))
       ((:OP_SEQ_LENGTH "Z3_OP_SEQ_LENGTH"))
       ((:OP_SEQ_INDEX "Z3_OP_SEQ_INDEX"))
       ((:OP_SEQ_LAST_INDEX "Z3_OP_SEQ_LAST_INDEX"))
       ((:OP_SEQ_TO_RE "Z3_OP_SEQ_TO_RE"))
       ((:OP_SEQ_IN_RE "Z3_OP_SEQ_IN_RE"))

       ;; strings
       ((:OP_STR_TO_INT "Z3_OP_STR_TO_INT"))
       ((:OP_INT_TO_STR "Z3_OP_INT_TO_STR"))

       ;; regular expressions
       ((:OP_RE_PLUS "Z3_OP_RE_PLUS"))
       ((:OP_RE_STAR "Z3_OP_RE_STAR"))
       ((:OP_RE_OPTION "Z3_OP_RE_OPTION"))
       ((:OP_RE_CONCAT "Z3_OP_RE_CONCAT"))
       ((:OP_RE_UNION "Z3_OP_RE_UNION"))
       ((:OP_RE_RANGE "Z3_OP_RE_RANGE"))
       ((:OP_RE_LOOP "Z3_OP_RE_LOOP"))
       ((:OP_RE_INTERSECT "Z3_OP_RE_INTERSECT"))
       ((:OP_RE_EMPTY_SET "Z3_OP_RE_EMPTY_SET"))
       ((:OP_RE_FULL_SET "Z3_OP_RE_FULL_SET"))
       ((:OP_RE_COMPLEMENT "Z3_OP_RE_COMPLEMENT"))

       ;; Auxiliary
       ((:OP_LABEL "Z3_OP_LABEL"))
       ((:OP_LABEL_LIT "Z3_OP_LABEL_LIT"))

       ;; Datatypes
       ((:OP_DT_CONSTRUCTOR "Z3_OP_DT_CONSTRUCTOR"))
       ((:OP_DT_RECOGNISER "Z3_OP_DT_RECOGNISER"))
       ((:OP_DT_IS "Z3_OP_DT_IS"))
       ((:OP_DT_ACCESSOR "Z3_OP_DT_ACCESSOR"))
       ((:OP_DT_UPDATE_FIELD "Z3_OP_DT_UPDATE_FIELD"))

       ;; Pseudo Booleans
       ((:OP_PB_AT_MOST "Z3_OP_PB_AT_MOST"))
       ((:OP_PB_AT_LEAST "Z3_OP_PB_AT_LEAST"))
       ((:OP_PB_LE "Z3_OP_PB_LE"))
       ((:OP_PB_GE "Z3_OP_PB_GE"))
       ((:OP_PB_EQ "Z3_OP_PB_EQ"))

       ;; Special relations
       ((:OP_SPECIAL_RELATION_LO "Z3_OP_SPECIAL_RELATION_LO"))
       ((:OP_SPECIAL_RELATION_PO "Z3_OP_SPECIAL_RELATION_PO"))
       ((:OP_SPECIAL_RELATION_PLO "Z3_OP_SPECIAL_RELATION_PLO"))
       ((:OP_SPECIAL_RELATION_TO "Z3_OP_SPECIAL_RELATION_TO"))
       ((:OP_SPECIAL_RELATION_TC "Z3_OP_SPECIAL_RELATION_TC"))
       ((:OP_SPECIAL_RELATION_TRC "Z3_OP_SPECIAL_RELATION_TRC"))


       ;; Floating-Point Arithmetic
       ((:OP_FPA_RM_NEAREST_TIES_TO_EVEN "Z3_OP_FPA_RM_NEAREST_TIES_TO_EVEN"))
       ((:OP_FPA_RM_NEAREST_TIES_TO_AWAY "Z3_OP_FPA_RM_NEAREST_TIES_TO_AWAY"))
       ((:OP_FPA_RM_TOWARD_POSITIVE "Z3_OP_FPA_RM_TOWARD_POSITIVE"))
       ((:OP_FPA_RM_TOWARD_NEGATIVE "Z3_OP_FPA_RM_TOWARD_NEGATIVE"))
       ((:OP_FPA_RM_TOWARD_ZERO "Z3_OP_FPA_RM_TOWARD_ZERO"))

       ((:OP_FPA_NUM "Z3_OP_FPA_NUM"))
       ((:OP_FPA_PLUS_INF "Z3_OP_FPA_PLUS_INF"))
       ((:OP_FPA_MINUS_INF "Z3_OP_FPA_MINUS_INF"))
       ((:OP_FPA_NAN "Z3_OP_FPA_NAN"))
       ((:OP_FPA_PLUS_ZERO "Z3_OP_FPA_PLUS_ZERO"))
       ((:OP_FPA_MINUS_ZERO "Z3_OP_FPA_MINUS_ZERO"))

       ((:OP_FPA_ADD "Z3_OP_FPA_ADD"))
       ((:OP_FPA_SUB "Z3_OP_FPA_SUB"))
       ((:OP_FPA_NEG "Z3_OP_FPA_NEG"))
       ((:OP_FPA_MUL "Z3_OP_FPA_MUL"))
       ((:OP_FPA_DIV "Z3_OP_FPA_DIV"))
       ((:OP_FPA_REM "Z3_OP_FPA_REM"))
       ((:OP_FPA_ABS "Z3_OP_FPA_ABS"))
       ((:OP_FPA_MIN "Z3_OP_FPA_MIN"))
       ((:OP_FPA_MAX "Z3_OP_FPA_MAX"))
       ((:OP_FPA_FMA "Z3_OP_FPA_FMA"))
       ((:OP_FPA_SQRT "Z3_OP_FPA_SQRT"))
       ((:OP_FPA_ROUND_TO_INTEGRAL "Z3_OP_FPA_ROUND_TO_INTEGRAL"))

       ((:OP_FPA_EQ "Z3_OP_FPA_EQ"))
       ((:OP_FPA_LT "Z3_OP_FPA_LT"))
       ((:OP_FPA_GT "Z3_OP_FPA_GT"))
       ((:OP_FPA_LE "Z3_OP_FPA_LE"))
       ((:OP_FPA_GE "Z3_OP_FPA_GE"))
       ((:OP_FPA_IS_NAN "Z3_OP_FPA_IS_NAN"))
       ((:OP_FPA_IS_INF "Z3_OP_FPA_IS_INF"))
       ((:OP_FPA_IS_ZERO "Z3_OP_FPA_IS_ZERO"))
       ((:OP_FPA_IS_NORMAL "Z3_OP_FPA_IS_NORMAL"))
       ((:OP_FPA_IS_SUBNORMAL "Z3_OP_FPA_IS_SUBNORMAL"))
       ((:OP_FPA_IS_NEGATIVE "Z3_OP_FPA_IS_NEGATIVE"))
       ((:OP_FPA_IS_POSITIVE "Z3_OP_FPA_IS_POSITIVE"))

       ((:OP_FPA_FP "Z3_OP_FPA_FP"))
       ((:OP_FPA_TO_FP "Z3_OP_FPA_TO_FP"))
       ((:OP_FPA_TO_FP_UNSIGNED "Z3_OP_FPA_TO_FP_UNSIGNED"))
       ((:OP_FPA_TO_UBV "Z3_OP_FPA_TO_UBV"))
       ((:OP_FPA_TO_SBV "Z3_OP_FPA_TO_SBV"))
       ((:OP_FPA_TO_REAL "Z3_OP_FPA_TO_REAL"))

       ((:OP_FPA_TO_IEEE_BV "Z3_OP_FPA_TO_IEEE_BV"))

       ((:OP_FPA_BVWRAP "Z3_OP_FPA_BVWRAP"))
       ((:OP_FPA_BV2RM "Z3_OP_FPA_BV2RM"))

       ((:OP_INTERNAL "Z3_OP_INTERNAL"))

       ((:OP_UNINTERPRETED "Z3_OP_UNINTERPRETED"))
       )

#|
   \brief The different kinds of parameters that can be associated with parameter sets.
   (see #Z3_mk_params).

    - Z3_PK_UINT integer parameters.
    - Z3_PK_BOOL boolean parameters.
    - Z3_PK_DOUBLE double parameters.
    - Z3_PK_SYMBOL symbol parameters.
    - Z3_PK_STRING string parameters.
    - Z3_PK_OTHER all internal parameter kinds which are not exposed in the API.
    - Z3_PK_INVALID invalid parameter.
|#
(cenum (param_kind)
       ((:PK_UINT "Z3_PK_UINT"))
       ((:PK_BOOL "Z3_PK_BOOL"))
       ((:PK_DOUBLE "Z3_PK_DOUBLE"))
       ((:PK_SYMBOL "Z3_PK_SYMBOL"))
       ((:PK_STRING "Z3_PK_STRING"))
       ((:PK_OTHER "Z3_PK_OTHER"))
       ((:PK_INVALID "Z3_PK_INVALID")))

#|
    \brief Z3 pretty printing modes (See #Z3_set_ast_print_mode).

   - Z3_PRINT_SMTLIB_FULL:   Print AST nodes in SMTLIB verbose format.
   - Z3_PRINT_LOW_LEVEL:     Print AST nodes using a low-level format.
   - Z3_PRINT_SMTLIB2_COMPLIANT: Print AST nodes in SMTLIB 2.x compliant format.
|#
(cenum (ast_print_mode)
       ((:PRINT_SMTLIB_FULL "Z3_PRINT_SMTLIB_FULL"))
       ((:PRINT_LOW_LEVEL "Z3_PRINT_LOW_LEVEL"))
       ((:PRINT_SMTLIB2_COMPLIANT "Z3_PRINT_SMTLIB2_COMPLIANT")))

#|
   \brief Z3 error codes (See #Z3_get_error_code).

   - Z3_OK:            No error.
   - Z3_SORT_ERROR:    User tried to build an invalid (type incorrect) AST.
   - Z3_IOB:           Index out of bounds.
   - Z3_INVALID_ARG:   Invalid argument was provided.
   - Z3_PARSER_ERROR:  An error occurred when parsing a string or file.
   - Z3_NO_PARSER:     Parser output is not available, that is, user didn't invoke #Z3_parse_smtlib2_string or #Z3_parse_smtlib2_file.
   - Z3_INVALID_PATTERN: Invalid pattern was used to build a quantifier.
   - Z3_MEMOUT_FAIL:   A memory allocation failure was encountered.
   - Z3_FILE_ACCESS_ERRROR: A file could not be accessed.
   - Z3_INVALID_USAGE:   API call is invalid in the current state.
   - Z3_INTERNAL_FATAL: An error internal to Z3 occurred.
   - Z3_DEC_REF_ERROR: Trying to decrement the reference counter of an AST that was deleted or the reference counter was not initialized with #Z3_inc_ref.
   - Z3_EXCEPTION:     Internal Z3 exception. Additional details can be retrieved using #Z3_get_error_msg.
|#
(cenum (error_code)
       ((:OK "Z3_OK"))
       ((:SORT_ERROR "Z3_SORT_ERROR"))
       ((:IOB "Z3_IOB"))
       ((:INVALID_ARG "Z3_INVALID_ARG"))
       ((:PARSER_ERROR "Z3_PARSER_ERROR"))
       ((:NO_PARSER "Z3_NO_PARSER"))
       ((:INVALID_PATTERN "Z3_INVALID_PATTERN"))
       ((:MEMOUT_FAIL "Z3_MEMOUT_FAIL"))
       ((:FILE_ACCESS_ERROR "Z3_FILE_ACCESS_ERROR"))
       ((:INTERNAL_FATAL "Z3_INTERNAL_FATAL"))
       ((:INVALID_USAGE "Z3_INVALID_USAGE"))
       ((:DEC_REF_ERROR "Z3_DEC_REF_ERROR"))
       ((:EXCEPTION "Z3_EXCEPTION")))

#|
   \brief A Goal is essentially a set of formulas.
   Z3 provide APIs for building strategies/tactics for solving and transforming Goals.
   Some of these transformations apply under/over approximations.

   - Z3_GOAL_PRECISE:    Approximations/Relaxations were not applied on the goal (sat and unsat answers were preserved).
   - Z3_GOAL_UNDER:      Goal is the product of a under-approximation (sat answers are preserved).
   - Z3_GOAL_OVER:       Goal is the product of an over-approximation (unsat answers are preserved).
   - Z3_GOAL_UNDER_OVER: Goal is garbage (it is the product of over- and under-approximations, sat and unsat answers are not preserved).
|#
(cenum (goal_prec)
       ((:GOAL_PRECISE "Z3_GOAL_PRECISE"))
       ((:GOAL_UNDER "Z3_GOAL_UNDER"))
       ((:GOAL_OVER "Z3_GOAL_OVER"))
       ((:GOAL_UNDER_OVER "Z3_GOAL_UNDER_OVER")))
