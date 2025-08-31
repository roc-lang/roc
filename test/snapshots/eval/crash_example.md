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
  (crash
    (str_literal_big "This is a crash statement")
  )
  (record_literal)
)
~~~
# FORMATTED
~~~roc
crash "This is a crash statement"
{}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.crash
    (Expr.str_literal_big)
  )
  (Expr.record_literal
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
