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
OpenRound Int Comma Int Comma Int Comma Int Comma Int Comma Int Comma Float Comma Float Comma Int Comma Int Comma Int Comma CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (num_literal_big big:<idx:0>)
  (num_literal_big big:<idx:5>)
  (num_literal_big big:<idx:10>)
  (num_literal_big big:<idx:15>)
  (num_literal_big big:<idx:20>)
  (num_literal_big big:<idx:25>)
  (frac_literal_big big:<idx:30>)
  (frac_literal_big big:<idx:37>)
  (num_literal_big big:<idx:44>)
  (num_literal_big big:<idx:55>)
  (num_literal_big big:<idx:66>)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
(
	0x42,
	0x42,
	0b01,
	0b01,
	0o42,
	0o42,
	0.1e42,
	0.1E42,
	0xDEADBEEF,
	0xdeadbeef,
	0xDeAdBeEf,
)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 13:1 to 13:1

**Parse Error**
at 13:2 to 13:2

# CANONICALIZE
~~~clojure
(Expr.tuple_literal
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.frac_literal_big big:<idx:30>)
  (Expr.frac_literal_big big:<idx:37>)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
