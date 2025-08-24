# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly OpenCurly Int CloseCurly CloseCurly String ~~~
# PARSE
~~~clojure
(block
  (block
    (block
      (num_literal_i32 0)
    )
  )
  (str_literal_small "")
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_044.md:1:20:1:21
PARSE ERROR - fuzz_crash_044.md:1:21:1:22
PARSE ERROR - fuzz_crash_044.md:1:22:1:23
PARSE ERROR - fuzz_crash_044.md:2:1:2:2
PARSE ERROR - fuzz_crash_044.md:2:2:2:3
PARSE ERROR - fuzz_crash_044.md:4:1:4:2
PARSE ERROR - fuzz_crash_044.md:4:2:4:2
PARSE ERROR - fuzz_crash_044.md:4:2:4:3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.block
      (Expr.num_literal_i32 0)
    )
  )
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Str")
~~~
# TYPES
~~~roc
~~~
