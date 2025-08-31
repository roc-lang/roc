# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]r:a	where
module(a).h:s
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []r : a where module(a).h : s
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "r")
    (type binop_where)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
