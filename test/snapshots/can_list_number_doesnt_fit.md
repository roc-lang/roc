# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1u8, 2u8, 300]
~~~
# TOKENS
~~~text
OpenSquare Int LowerIdent Comma Int LowerIdent Comma Int CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
)
~~~
# FORMATTED
~~~roc
[1]
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:3

# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
(expr :tag list_literal :type "List(_elem)")
~~~
# TYPES
~~~roc
List(_elem)
~~~
