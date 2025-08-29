# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# TOKENS
~~~text
KwApp OpenSquare CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly OpBar OpenRound Int Comma CloseRound OpOr OpBar Int ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
app { f: "" platform [] }

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:29 to 1:29

**Parse Error**
at 1:29 to 1:29

**Parse Error**
at 1:29 to 1:29

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
