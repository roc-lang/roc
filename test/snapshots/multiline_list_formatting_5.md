# META
~~~ini
description=multiline_list_formatting (5)
type=expr
~~~
# SOURCE
~~~roc
[1, 2, # Foo
  3]
~~~
# TOKENS
~~~text
OpenSquare Int Comma Int Comma Int CloseSquare ~~~
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
[1, 2, # Foo
3]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_or)
~~~
# SOLVED
~~~clojure
(expr :tag binop_or :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
