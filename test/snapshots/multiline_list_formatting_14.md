# META
~~~ini
description=multiline_list_formatting (14)
type=expr
~~~
# SOURCE
~~~roc
[ # Open
	1, # First

	# A comment in the middle

	2, # Second
	# This comment has no blanks around it
	3, # Third
]
~~~
# TOKENS
~~~text
OpenSquare LineComment Int Comma LineComment BlankLine LineComment BlankLine Int Comma LineComment LineComment Int Comma LineComment CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
  (num_literal_i32 2)
  (num_literal_i32 3)
)
~~~
# FORMATTED
~~~roc
[ # Open
	1, # First
	# A comment in the middle
	2, # Second
	# This comment has no blanks around it
	3, # Third
]
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
; Total type variables: 5
(var #0 _)
(var #1 Num *)
(var #2 Num *)
(var #3 Num *)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
