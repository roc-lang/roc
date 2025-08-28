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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
