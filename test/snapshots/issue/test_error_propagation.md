# META
~~~ini
description=Test error propagation - aliases that reference error types should not propagate errors
type=file
~~~
# SOURCE
~~~roc
module []

BadBase := _

GoodAlias := BadBase

value : GoodAlias
value = "test"
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine UpperIdent OpColonEqual UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon_equals
    (uc "BadBase")
    (underscore)
  )
  (binop_colon_equals
    (uc "GoodAlias")
    (uc "BadBase")
  )
  (binop_colon
    (lc "value")
    (uc "GoodAlias")
  )
  (binop_equals
    (lc "value")
    (str_literal_small "test")
  )
)
~~~
# FORMATTED
~~~roc
module []

BadBase := _
GoodAlias := BadBase
value : GoodAlias
value = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - test_error_propagation.md:1:1:1:1
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.opaque_type)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "value"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "value"))
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #11)
(var #11 Str)
(var #12 _)
(var #13 _)
~~~
# TYPES
~~~roc
value : Str
~~~
