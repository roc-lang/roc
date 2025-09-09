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
; Total type variables: 16
(var #0 _)
(var #1 Num *)
(var #2 -> #11)
(var #3 -> #1)
(var #4 -> #12)
(var #5 -> #1)
(var #6 -> #5)
(var #7 -> #13)
(var #8 -> #1)
(var #9 -> #14)
(var #10 -> #15)
(var #11 List #1)
(var #12 -> #11)
(var #13 -> #11)
(var #14 -> #11)
(var #15 List #2)
~~~
# TYPES
~~~roc
~~~
