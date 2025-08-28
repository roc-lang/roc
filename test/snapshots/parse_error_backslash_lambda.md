# META
~~~ini
description=Backslash is not valid lambda syntax - helpful error
type=expr
~~~
# SOURCE
~~~roc
\x -> x + 1
~~~
# TOKENS
~~~text
OpBackslash LowerIdent OpArrow LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(binop_thin_arrow
  (malformed malformed:backslash_not_valid_lambda_syntax)
  (binop_plus
    (lc "x")
    (num_literal_i32 1)
  )
)
~~~
# FORMATTED
~~~roc
 -> x + 1
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:2 to 1:2

**Unsupported Node**
at 1:4 to 1:6

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
