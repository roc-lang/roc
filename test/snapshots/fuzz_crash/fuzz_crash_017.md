# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# TOKENS
~~~text
LowerIdent OpAssign String LowerIdent OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "me")
    (str_literal_small "luc")
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
MISSING HEADER - fuzz_crash_017.md:1:1:1:3
PARSE ERROR - fuzz_crash_017.md:1:4:1:5
PARSE ERROR - fuzz_crash_017.md:1:6:1:7
PARSE ERROR - fuzz_crash_017.md:1:7:1:10
PARSE ERROR - fuzz_crash_017.md:1:10:1:11
PARSE ERROR - fuzz_crash_017.md:2:7:2:8
UNRECOGNIZED SYNTAX - fuzz_crash_017.md:2:7:2:20
# PROBLEMS
**Parse Error**
at 2:7 to 2:7

**Unsupported Node**
at 2:7 to 2:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
