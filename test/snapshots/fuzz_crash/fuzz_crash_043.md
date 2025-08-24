# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0}0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpenCurly LowerIdent OpColon Int CloseCurly Int ~~~
# PARSE
~~~clojure
(block
  (block
    (binop_colon
      (lc "o")
      (num_literal_i32 0)
    )
  )
  (num_literal_i32 0)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_043.md:1:20:1:21
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_043.md:2:3:2:4
PARSE ERROR - fuzz_crash_043.md:2:4:2:5
PARSE ERROR - fuzz_crash_043.md:2:5:2:6
MALFORMED TYPE - fuzz_crash_043.md:2:3:2:4
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.record_literal
    (Expr.binop_colon
      (Expr.lookup "o")
      (Expr.num_literal_i32 0)
    )
  )
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Num(_size)")
~~~
# TYPES
~~~roc
~~~
