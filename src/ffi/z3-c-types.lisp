;; SPDX-FileCopyrightText: Copyright (c) 2020 Andrew T. Walter <me@atwalter.com>
;; SPDX-License-Identifier: MIT
(in-package :z3-c-types)

(define-foreign-type symbol-type ()
  ()
  (:actual-type Z3_symbol)
  (:simple-parser sym))

(define-foreign-type config-type ()
  ()
  (:actual-type Z3_config)
  (:simple-parser config))

(define-foreign-type context-type ()
  ()
  (:actual-type Z3_context)
  (:simple-parser context))

(define-foreign-type sort-type ()
  ()
  (:actual-type Z3_sort)
  (:simple-parser sort))

(define-foreign-type func-decl-type ()
  ()
  (:actual-type Z3_func_decl)
  (:simple-parser func-decl))

(define-foreign-type ast-type ()
  ()
  (:actual-type Z3_ast)
  (:simple-parser ast))

(define-foreign-type app-type ()
  ()
  (:actual-type Z3_app)
  (:simple-parser app))

(define-foreign-type pattern-type ()
  ()
  (:actual-type Z3_pattern)
  (:simple-parser pattern))

(define-foreign-type constructor-type ()
  ()
  (:actual-type Z3_constructor)
  (:simple-parser constructor))

(define-foreign-type constructor-list-type ()
  ()
  (:actual-type Z3_constructor_list)
  (:simple-parser constructor-list))

(define-foreign-type params-type ()
  ()
  (:actual-type Z3_params)
  (:simple-parser params))

(define-foreign-type param-descrs-type ()
  ()
  (:actual-type Z3_param_descrs)
  (:simple-parser param-descrs))

(define-foreign-type parser-context-type ()
  ()
  (:actual-type Z3_parser_context)
  (:simple-parser parser-context))

(define-foreign-type model-type ()
  ()
  (:actual-type Z3_model)
  (:simple-parser model))

(define-foreign-type func-interp-type ()
  ()
  (:actual-type Z3_func_interp)
  (:simple-parser func-interp))

(define-foreign-type func-entry-type ()
  ()
  (:actual-type Z3_func_entry)
  (:simple-parser func-entry))

(define-foreign-type fixedpoint-type ()
  ()
  (:actual-type Z3_fixedpoint)
  (:simple-parser fixedpoint))

(define-foreign-type optimize-type ()
  ()
  (:actual-type Z3_optimize)
  (:simple-parser optimize))

(define-foreign-type ast-vector-type ()
  ()
  (:actual-type Z3_ast_vector)
  (:simple-parser ast-vector))

(define-foreign-type ast-map-type ()
  ()
  (:actual-type Z3_ast_map)
  (:simple-parser ast-map))

(define-foreign-type goal-type ()
  ()
  (:actual-type Z3_goal)
  (:simple-parser goal))

(define-foreign-type tactic-type ()
  ()
  (:actual-type Z3_tactic)
  (:simple-parser tactic))

(define-foreign-type probe-type ()
  ()
  (:actual-type Z3_probe)
  (:simple-parser probe))

(define-foreign-type apply-result-type ()
  ()
  (:actual-type Z3_apply_result)
  (:simple-parser apply-result))

(define-foreign-type solver-type ()
  ()
  (:actual-type Z3_solver)
  (:simple-parser solver))

(define-foreign-type solver-callback-type ()
  ()
  (:actual-type Z3_solver_callback)
  (:simple-parser solver-callback))

(define-foreign-type stats-type ()
  ()
  (:actual-type Z3_stats)
  (:simple-parser stats))
