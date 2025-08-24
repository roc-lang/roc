# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||(|(l888888888|
~~~
# TOKENS
~~~text
OpOr OpenRound OpBar OpenRound LowerIdent OpBar ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (malformed malformed:expr_unexpected_token)
    (lambda
      (body
        (malformed malformed:expr_unexpected_token)
      )
      (args
        (lc "l888888888")
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
MISSING HEADER - fuzz_crash_012.md:1:1:1:2
PARSE ERROR - fuzz_crash_012.md:1:2:1:3
PARSE ERROR - fuzz_crash_012.md:1:3:1:4
PARSE ERROR - fuzz_crash_012.md:1:4:1:5
PARSE ERROR - fuzz_crash_012.md:1:5:1:6
PARSE ERROR - fuzz_crash_012.md:1:6:1:16
PARSE ERROR - fuzz_crash_012.md:1:16:1:17
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

**Parse Error**
at 1:16 to 1:16

**Parse Error**
at 1:17 to 1:17

**Parse Error**
at 1:3 to 1:17

**Unsupported Node**
at 1:3 to 1:3

**Unsupported Node**
at 1:4 to 1:17

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
