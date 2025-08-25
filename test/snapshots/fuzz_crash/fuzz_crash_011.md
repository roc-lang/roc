# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# TOKENS
~~~text
KwModule UpperIdent CloseSquare UpperIdent ~~~
# PARSE
~~~clojure
(block
  (uc "P")
  (malformed malformed:expr_unexpected_token)
  (uc "F")
)
~~~
# FORMATTED
~~~roc
module [
	P,
	],
	F,
]

P]
F
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:8

**Parse Error**
at 1:9 to 1:9

**Unsupported Node**
at 1:9 to 1:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
