# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = 500
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x : U8
x = 500
~~~
# EXPECTED
NUMBER DOES NOT FIT IN TYPE - u8_annotation_large_value.md:4:5:4:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "x")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 500)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
