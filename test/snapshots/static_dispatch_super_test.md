# META
~~~ini
description=Dot access super test
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
~~~
# TOKENS
~~~text
LowerIdent OpenRound LowerIdent CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpenRound CloseRound OpQuestion Dot LowerIdent OpQuestion ~~~
# PARSE
~~~clojure
(apply_lc
  (lc "some_fn")
  (lc "arg1")
)
~~~
# FORMATTED
~~~roc
some_fn(arg1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_thick_arrow)
~~~
# SOLVED
~~~clojure
(expr :tag binop_thick_arrow :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
