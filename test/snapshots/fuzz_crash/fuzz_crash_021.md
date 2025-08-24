# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# TOKENS
~~~text
UpperIdent OpSlash LowerIdent Dot LowerIdent MalformedString UpperIdent OpenRound LowerIdent Comma LowerIdent OpPlus OpColon OpenRound ~~~
# PARSE
~~~clojure
(block
  (binop_slash
    (uc "Fli")
    (binop_pipe
      (lc "main")
      (dot_lc "roc")
    )
  )
  (malformed malformed:expr_unexpected_token)
  (apply_uc
    (uc "Pair")
    (tuple_literal
      (lc "a")
      (binop_plus
        (lc "b")
        (apply_anon
          (malformed malformed:expr_unexpected_token)
        )
      )
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
MISSING HEADER - fuzz_crash_021.md:1:1:1:4
PARSE ERROR - fuzz_crash_021.md:1:4:1:5
PARSE ERROR - fuzz_crash_021.md:1:5:1:9
PARSE ERROR - fuzz_crash_021.md:1:9:1:13
PARSE ERROR - fuzz_crash_021.md:1:13:1:14
PARSE ERROR - fuzz_crash_021.md:1:14:1:16
PARSE ERROR - fuzz_crash_021.md:1:16:1:16
PARSE ERROR - fuzz_crash_021.md:3:1:3:5
PARSE ERROR - fuzz_crash_021.md:3:15:3:15
MALFORMED TYPE - fuzz_crash_021.md:3:14:3:15
# PROBLEMS
**Parse Error**
at 1:13 to 1:13

**Parse Error**
at 3:12 to 3:12

**Parse Error**
at 3:12 to 3:15

**Parse Error**
at 3:1 to 3:15

**Pattern in Expression Context**
at 1:1 to 1:4

**Unsupported Node**
at 1:13 to 1:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_slash
    (Expr.malformed)
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
