# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
 f{o,
     ]

foo =

    "onmo %
~~~
# TOKENS
~~~text
LowerIdent OpenCurly LowerIdent Comma CloseSquare LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (lc "f")
  (record_literal
    (lc "o")
    (malformed malformed:expr_unexpected_token)
  )
  (binop_equals
    (lc "foo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_009.md:1:2:1:3
PARSE ERROR - fuzz_crash_009.md:1:3:1:4
PARSE ERROR - fuzz_crash_009.md:1:4:1:5
PARSE ERROR - fuzz_crash_009.md:1:5:1:6
PARSE ERROR - fuzz_crash_009.md:2:6:2:7
PARSE ERROR - fuzz_crash_009.md:6:12:6:12
# PROBLEMS
**Parse Error**
at 2:6 to 2:6

**Parse Error**
at 1:3 to 4:1

**Parse Error**
at 6:5 to 6:5

**Unsupported Node**
at 2:6 to 2:6

**Unsupported Node**
at 6:5 to 6:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "f")
  (Expr.record_literal
    (Expr.lookup "o")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
