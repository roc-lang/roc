# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# TOKENS
~~~text
UpperIdent ~~~
# PARSE
~~~clojure
(block
  (uc "F")
)
~~~
# FORMATTED
~~~roc
F
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_004.md:1:1:1:2
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.tag_no_args)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 3
(var #0 _)
(var #1 _)
(var #2 _)
~~~
# TYPES
~~~roc
~~~
