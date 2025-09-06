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
(app-header
  (packages
    (binop_colon
      (lc "f")
      (binop_platform
        (str_literal_small "")
        (block)
      )
    )
))
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
app { f: "" platform [] }

{
	o : 0
}
0
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
    (Expr.record_field
      (Expr.malformed)
      (Expr.num_literal_i32 0)
    )
  )
  (Expr.num_literal_i32 0)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 Num *)
(var #8 _)
(var #9 -> #12)
(var #10 Num *)
(var #11 _)
(var #12 {})
~~~
# TYPES
~~~roc
~~~
