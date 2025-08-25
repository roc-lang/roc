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
  (tuple_literal
    (list_literal
      (num_literal_i32 1)
    )
    (list_literal
      (num_literal_i32 2)
    )
    (list_literal
      (tuple_literal
        (num_literal_i32 3)
        (num_literal_i32 4)
        (malformed malformed:expr_unexpected_token)
        (list_literal
          (num_literal_i32 5)
        )
        (malformed malformed:expr_unexpected_token)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
[
	([1], [2], [
		(3, 4, <malformed>, [5], <malformed>),
	]),
]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:2 to 7:2

**Parse Error**
at 9:1 to 9:1

**Parse Error**
at 4:2 to 9:2

**Parse Error**
at 1:1 to 9:2

**Unsupported Node**
at 1:1 to 9:1

# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
