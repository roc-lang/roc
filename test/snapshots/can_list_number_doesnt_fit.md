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
NO CHANGE
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_number_doesnt_fit.md:1:7:1:7
# PROBLEMS
**Parse Error**
at 1:1 to 1:3

**Unsupported Node**
at 1:1 to 1:4

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
