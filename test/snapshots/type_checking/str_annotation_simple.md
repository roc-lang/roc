# META
~~~ini
description=Simple Str type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Str
x = "hello"
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x : Str
x = "hello"
~~~
# EXPECTED
NIL
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
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
