# META
~~~ini
description=Module dot malformed (should error)
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# TOKENS
~~~text
UpperIdent Dot Int ~~~
# PARSE
~~~clojure
(binop_pipe
  (uc "I")
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
I | 5
~~~
# EXPECTED
PARSE ERROR - module_dot_tuple.md:1:2:1:4
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_pipe)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
