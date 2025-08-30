# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}{0      0)(0}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpenCurly Int Int CloseRound OpenRound Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
module [}]

{
	0
	0
	)(0)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:9

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 1:18 to 1:19

**Parse Error**
at 1:18 to 1:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.num_literal_i32 0)
    (Expr.num_literal_i32 0)
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
