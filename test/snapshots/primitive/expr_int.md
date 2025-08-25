# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (num_literal_i32 42)
  )
)
~~~
# FORMATTED
~~~roc
module [
	foo,
]

foo = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
