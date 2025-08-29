# META
~~~ini
description=Simple when expression
type=expr
~~~
# SOURCE
~~~roc
when x is
    Ok(value) -> value
    Err(msg) -> msg
~~~
# TOKENS
~~~text
LowerIdent LowerIdent LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(lc "when")
~~~
# FORMATTED
~~~roc
when
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lookup "when")
~~~
# SOLVED
~~~clojure
(expr :tag lookup :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
