# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# TOKENS
~~~text
KwIf UpperIdent Dot UpperIdent UpperIdent OpenRound Int CloseRound KwElse UpperIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(if_else <6 branches>)
~~~
# FORMATTED
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Unsupported Node**
at 1:4 to 1:8

# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "[]_others")
~~~
# TYPES
~~~roc
[]_others
~~~
