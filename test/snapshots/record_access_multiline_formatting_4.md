# META
~~~ini
description=record_access_multiline_formatting (4)
type=expr
~~~
# SOURCE
~~~roc
some_fn(arg1)? # Comment 1
	.static_dispatch_method()? # Comment 2
	.next_static_dispatch_method()? # Comment 3
	.record_field?
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
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
