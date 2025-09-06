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
{}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
)
~~~
# SOLVED
~~~clojure
; Total type variables: 3
(var #0 _)
(var #1 -> #2)
(var #2 {})
~~~
# TYPES
~~~roc
~~~
