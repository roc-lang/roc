# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
MyTag
~~~
# TOKENS
~~~text
UpperIdent ~~~
# PARSE
~~~clojure
(uc "MyTag")
~~~
# FORMATTED
~~~roc
MyTag
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.tag_no_args)
~~~
# SOLVED
~~~clojure
; Total type variables: 2
(var #0 _)
(var #1 _)
~~~
# TYPES
~~~roc
~~~
