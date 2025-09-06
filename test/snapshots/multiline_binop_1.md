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
Int LineComment OpPlus LineComment BlankLine LineComment BlankLine Int LineComment OpStar LineComment Int ~~~
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
1 + # One
# Plus

# A comment in between

2 * # Two
# Times
3
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
~~~
# TYPES
~~~roc
# No header found
~~~
