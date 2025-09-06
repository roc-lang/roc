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
; Total type variables: 11
(var #0 _)
(var #1 Num *)
(var #2 _)
(var #3 Num *)
(var #4 _)
(var #5 Num *)
(var #6 Num *)
(var #7 _)
(var #8 Num *)
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
~~~
