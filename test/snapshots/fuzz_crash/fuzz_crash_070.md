# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]()0     .t
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound CloseRound Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (tuple_literal)
  (binop_pipe
    (num_literal_i32 0)
    (dot_lc "t")
  )
)
~~~
# FORMATTED
~~~roc
module []

()
0 | .t
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_070.md:1:9:1:10
PARSE ERROR - fuzz_crash_070.md:1:10:1:11
PARSE ERROR - fuzz_crash_070.md:1:11:1:12
PARSE ERROR - fuzz_crash_070.md:1:17:1:19
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.tuple_literal
  )
  (Expr.binop_pipe)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 -> #6)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 tuple)
~~~
# TYPES
~~~roc
~~~
