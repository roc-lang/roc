# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C(2))
~~~
# TOKENS
~~~text
OpBang OpenRound UpperIdent OpenRound Int CloseRound CloseRound ~~~
# PARSE
~~~clojure
(unary_not <unary_op>)
~~~
# FORMATTED
~~~roc
!C(2)
~~~
# EXPECTED
TYPE MISMATCH - not_tag.md:1:1:1:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_not)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 -> #5)
(var #2 Num *)
(var #3 _)
(var #4 -> #3)
(var #5 fn_pure)
~~~
# TYPES
~~~roc
~~~
