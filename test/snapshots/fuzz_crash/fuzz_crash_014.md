# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# TOKENS
~~~text
MalformedNumberNoDigits Dot Int MalformedNumberNoDigits LowerIdent Int LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (malformed malformed:expr_unexpected_token)
    (num_literal_i32 0)
  )
  (malformed malformed:expr_unexpected_token)
  (lc "u22")
  (num_literal_i32 0)
  (lc "u22")
)
~~~
# FORMATTED
~~~roc
 | 0
u22
0u22
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

**Parse Error**
at 2:1 to 2:1

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
