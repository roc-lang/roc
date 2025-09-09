# META
~~~ini
description=multiline_list_formatting (7)
type=expr
~~~
# SOURCE
~~~roc
[
	1,
	2, # Foo
	3,
]
~~~
# TOKENS
~~~text
OpenSquare Int Comma Int Comma LineComment Int Comma CloseSquare ~~~
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
[
	1,
	2, # Foo
	3,
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
; Total type variables: 6
(var #0 _)
(var #1 Num *)
(var #2 -> #1)
(var #3 -> #1)
(var #4 -> #5)
(var #5 List #1)
~~~
# TYPES
~~~roc
~~~
