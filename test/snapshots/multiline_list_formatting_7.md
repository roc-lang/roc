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
