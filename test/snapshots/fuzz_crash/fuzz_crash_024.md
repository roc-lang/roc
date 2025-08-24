# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [module ] { pf: platform ".-/main._]where # A

#el
var t= ]

#el
var t= 0
~~~
# TOKENS
~~~text
KwModule OpenSquare KwModule CloseSquare OpenCurly LowerIdent OpColon KwPlatform MalformedString KwVar LowerIdent OpAssign CloseSquare KwVar LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (block
    (binop_colon
      (lc "pf")
      (malformed malformed:expr_unexpected_token)
    )
    (malformed malformed:expr_unexpected_token)
    (binop_equals
      (var_lc "t")
      (malformed malformed:expr_unexpected_token)
    )
    (binop_equals
      (var_lc "t")
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_024.md:1:9:1:15
PARSE ERROR - fuzz_crash_024.md:1:18:1:19
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_024.md:1:24:1:32
PARSE ERROR - fuzz_crash_024.md:1:33:1:34
PARSE ERROR - fuzz_crash_024.md:1:34:1:53
PARSE ERROR - fuzz_crash_024.md:1:53:1:53
PARSE ERROR - fuzz_crash_024.md:4:1:4:4
UNEXPECTED TOKEN IN EXPRESSION - fuzz_crash_024.md:4:8:4:9
PARSE ERROR - fuzz_crash_024.md:7:1:7:4
MALFORMED TYPE - fuzz_crash_024.md:1:24:1:32
UNRECOGNIZED SYNTAX - fuzz_crash_024.md:4:8:4:9
DUPLICATE DEFINITION - fuzz_crash_024.md:7:5:7:6
# PROBLEMS
**Parse Error**
at 1:9 to 1:9

**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:33 to 1:33

**Parse Error**
at 4:8 to 4:8

**Parse Error**
at 1:18 to 7:9

**Unsupported Node**
at 1:24 to 1:24

**Unsupported Node**
at 1:33 to 1:33

**Unsupported Node**
at 4:8 to 4:8

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.block
    (Expr.binop_colon
      (Expr.lookup "pf")
      (Expr.malformed)
    )
    (Expr.malformed)
    (Expr.malformed)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
