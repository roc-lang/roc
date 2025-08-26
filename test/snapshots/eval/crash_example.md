# META
~~~ini
description=Expression with a crash statement
type=expr
~~~
# SOURCE
~~~roc
{
    crash "This is a crash statement"
    {}
}
~~~
# TOKENS
~~~text
OpenCurly KwCrash String OpenCurly CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (crash <statement>)
  (record_literal)
)
~~~
# FORMATTED
~~~roc
crash "This is a crash statement"
{  }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_double_slash)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
