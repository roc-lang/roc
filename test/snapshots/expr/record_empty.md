# META
~~~ini
description=Empty record expression
type=expr
~~~
# SOURCE
~~~roc
{}
~~~
# TOKENS
~~~text
OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(record_literal)
~~~
# FORMATTED
~~~roc
{  }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
