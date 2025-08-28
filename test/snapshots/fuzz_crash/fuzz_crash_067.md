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
module []

f = 
{
	crash 1
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:5 to 3:5

# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
