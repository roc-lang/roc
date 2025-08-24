# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# TOKENS
~~~text
UpperIdent OpenCurly LowerIdent Comma CloseSquare LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (uc "H")
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
ASCII CONTROL CHARACTER - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_010.md:1:1:1:2
PARSE ERROR - fuzz_crash_010.md:1:2:1:3
PARSE ERROR - fuzz_crash_010.md:1:3:1:4
PARSE ERROR - fuzz_crash_010.md:1:4:1:5
PARSE ERROR - fuzz_crash_010.md:2:6:2:7
PARSE ERROR - fuzz_crash_010.md:5:35:5:35
# PROBLEMS
**Parse Error**
at 2:6 to 2:6

**Parse Error**
at 1:2 to 3:1

**Parse Error**
at 5:5 to 5:5

**Unsupported Node**
at 2:6 to 2:6

**Unsupported Node**
at 5:5 to 5:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_tag)
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
