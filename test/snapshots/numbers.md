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
  (num_literal_big big:<idx:77>)
  (num_literal_big big:<idx:82>)
  (num_literal_big big:<idx:87>)
  (num_literal_big big:<idx:92>)
  (num_literal_big big:<idx:97>)
  (num_literal_big big:<idx:102>)
  (frac_literal_big big:<idx:107>)
  (frac_literal_big big:<idx:114>)
  (num_literal_big big:<idx:121>)
  (num_literal_big big:<idx:132>)
  (num_literal_big big:<idx:143>)
)
~~~
# FORMATTED
~~~roc
(0x42, 0x42, 0b01, 0b01, 0o42, 0o42, 0.1e42, 0.1E42, 0xDEADBEEF, 0xdeadbeef, 0xDeAdBeEf)
~~~
# EXPECTED
UPPERCASE BASE - :0:0:0:0
UPPERCASE BASE - :0:0:0:0
UPPERCASE BASE - :0:0:0:0
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.tuple_literal
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.frac_literal_big big:<idx:107>)
  (Expr.frac_literal_big big:<idx:114>)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
  (Expr.num_literal_big)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 Num *)
(var #2 Num *)
(var #3 Num *)
(var #4 Num *)
(var #5 Num *)
(var #6 Num *)
(var #7 F64)
(var #8 F64)
(var #9 Num *)
(var #10 Num *)
(var #11 Num *)
(var #12 -> #13)
(var #13 tuple)
~~~
# TYPES
~~~roc
~~~
