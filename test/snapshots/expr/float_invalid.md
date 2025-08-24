# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# TOKENS
~~~text
Float Dot Int ~~~
# PARSE
~~~clojure
(binop_pipe
  (frac_literal_small 3.14)
  (num_literal_i32 15)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - float_invalid.md:1:5:1:8
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:8

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
