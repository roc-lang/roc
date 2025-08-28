# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# TOKENS
~~~text
LowerIdent OpAssign String LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "me")
    (str_literal_small "luc")
  )
  (binop_equals
    (lc "foo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
me = "luc"
foo = 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:7 to 2:7

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
