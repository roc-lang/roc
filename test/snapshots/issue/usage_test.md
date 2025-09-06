# META
~~~ini
description=Test if usage affects error type conversion
type=file
~~~
# SOURCE
~~~roc
module []

UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine UpperIdent OpColonEqual Underscore BlankLine UpperIdent OpColonEqual Underscore BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon_equals
    (uc "UnusedType")
    (underscore)
  )
  (binop_colon_equals
    (uc "UsedType")
    (underscore)
  )
  (binop_colon
    (lc "value")
    (uc "UsedType")
  )
  (binop_equals
    (lc "value")
    (num_literal_i32 42)
  )
)
~~~
# FORMATTED
~~~roc
module []

UnusedType := _
UsedType := _
value : UsedType
value = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
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
    (Expr.num_literal_i32 42)
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
(var #11 Num *)
(var #12 _)
(var #13 _)
~~~
# TYPES
~~~roc
value : Num(_size)
~~~
