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
**Unsupported Node**
at 2:5 to 2:38

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.record_literal
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "{}")
~~~
# TYPES
~~~roc
~~~
