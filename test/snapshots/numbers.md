# META
~~~ini
description=Number formats
type=expr
~~~
# SOURCE
~~~roc
(
    0X42,
    0x42,
    0B01,
    0b01,
    0O42,
    0o42,
    0.1e42,
    0.1E42,
    0xDEADBEEF,
    0xdeadbeef,
    0xDeAdBeEf,
)
~~~
# TOKENS
~~~text
OpenRound Int UpperIdent Comma Int LowerIdent Comma Int UpperIdent Comma Int LowerIdent Comma Int UpperIdent Comma Int LowerIdent Comma Float Comma Float Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma CloseRound ~~~
# PARSE
~~~clojure
(num_literal_i32 0)
~~~
# FORMATTED
~~~roc
0
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:6 to 2:6

# CANONICALIZE
~~~clojure
(Expr.binop_star)
~~~
# SOLVED
~~~clojure
(expr :tag binop_star :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
