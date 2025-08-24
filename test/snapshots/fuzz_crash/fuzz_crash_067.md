# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

f = || {
    crash 1
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign OpOr OpenCurly KwCrash Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "f")
    (malformed malformed:expr_unexpected_token)
  )
  (block
    (crash <statement>)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
CRASH EXPECTS STRING - fuzz_crash_067.md:4:5:4:12
# PROBLEMS
**Parse Error**
at 3:5 to 3:5

**Unsupported Node**
at 3:5 to 3:5

**Unsupported Node**
at 4:5 to 4:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.block
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
