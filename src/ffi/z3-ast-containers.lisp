(in-package :z3-c)

(defcfun "Z3_mk_ast_vector" Z3_ast_vector
  "Return an empty AST vector.
   \remark Reference counting must be used to manage AST vectors, even when the Z3_context was
    created using #Z3_mk_context instead of #Z3_mk_context_rc."
  (c Z3_context))

(defcfun "Z3_ast_vector_inc_ref" :void
  "Increment the reference counter of the given AST vector."
  (c Z3_context)
  (v Z3_ast_vector))

(defcfun "Z3_ast_vector_dec_ref" :void
  "Decrement the reference counter of the given AST vector."
  (c Z3_context)
  (v Z3_ast_vector))

(defcfun "Z3_ast_vector_size" :uint
  "Return the size of the given AST vector."
  (c Z3_context)
  (v Z3_ast_vector))

(defcfun "Z3_ast_vector_get" Z3_ast
  "Return the AST at position `i` in the AST vector `v`."
  (c Z3_context)
  (v Z3_ast_vector)
  (i :uint)) ;; i < Z3_ast_vector_size(c,v)

(defcfun "Z3_ast_vector_set" :void
  "Update position `i` of the AST vector `v` with the AST `a`."
  (c Z3_context)
  (v Z3_ast_vector)
  (i :uint) ;; i < Z3_ast_vector_size(c,v)
  (a Z3_ast))

(defcfun "Z3_ast_vector_resize" :void
  "Resize the AST vector `v`."
  (c Z3_context)
  (v Z3_ast_vector)
  (n :uint))

(defcfun "Z3_ast_vector_push" :void
  "Add the AST \c a in the end of the AST vector \c v. The size of \c v is increased by one."
  (c Z3_context)
  (v Z3_ast_vector)
  (a Z3_ast))

(defcfun "Z3_ast_vector_translate" Z3_ast_vector
  "Translate the AST vector \c v from context \c s into an AST vector in context \c t."
  (source Z3_context)
  (v Z3_ast_vector)
  (target Z3_context))

(defcfun "Z3_ast_vector_to_string" :string
  "Convert the AST vector `v` into a string."
  (c Z3_context)
  (v Z3_ast_vector))
