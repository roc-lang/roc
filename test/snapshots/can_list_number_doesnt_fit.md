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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
u8
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:3

**Parse Error**
at 1:3 to 1:5

**Unsupported Node**
at 1:3 to 1:5

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
