# META
~~~ini
description=multiline_binop (1)
type=expr
~~~
# SOURCE
~~~roc
1 # One
	+ # Plus

	# A comment in between

	2 # Two
		* # Times
		3
~~~
# TOKENS
~~~text
Int OpPlus Int OpStar Int ~~~
# PARSE
~~~clojure
(binop_plus
  (num_literal_i32 1)
  (binop_star
    (num_literal_i32 2)
    (num_literal_i32 3)
  )
)
~~~
# FORMATTED
~~~roc
1 + 2 * 3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_plus
  (Expr.num_literal_i32 1)
  (Expr.binop_star
    (Expr.num_literal_i32 2)
    (Expr.num_literal_i32 3)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_plus :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~
