# META
~~~ini
description=multiline_list_formatting (11)
type=expr
~~~
# SOURCE
~~~roc
[
	[1],
	[2],
	[
		3,
		4,
	],
	[5],
]
~~~
# TOKENS
~~~text
OpenSquare OpenSquare Int CloseSquare Comma OpenSquare Int CloseSquare Comma OpenSquare Int Comma Int Comma CloseSquare Comma OpenSquare Int CloseSquare Comma CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (list_literal
    (num_literal_i32 1)
  )
  (list_literal
    (num_literal_i32 2)
  )
  (list_literal
    (num_literal_i32 3)
    (num_literal_i32 4)
  )
  (list_literal
    (num_literal_i32 5)
  )
)
~~~
# FORMATTED
~~~roc
[[1], [2], [
	3,
	4,
], [5]]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
