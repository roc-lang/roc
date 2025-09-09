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
OpenRound IntBase Comma IntBase Comma IntBase Comma IntBase Comma IntBase Comma IntBase Comma Float Comma Float Comma IntBase Comma IntBase Comma IntBase Comma CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (int_literal_big int:<idx:63>)
  (int_literal_big int:<idx:66>)
  (int_literal_big int:<idx:69>)
  (int_literal_big int:<idx:71>)
  (int_literal_big int:<idx:73>)
  (int_literal_big int:<idx:76>)
  (frac_literal_big frac:<idx:79>)
  (frac_literal_big frac:<idx:86>)
  (int_literal_big int:<idx:93>)
  (int_literal_big int:<idx:104>)
  (int_literal_big int:<idx:115>)
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
  (Expr.int_literal_big)
  (Expr.int_literal_big)
  (Expr.int_literal_big)
  (Expr.int_literal_big)
  (Expr.int_literal_big)
  (Expr.int_literal_big)
  (Expr.frac_literal_big big:<idx:79>)
  (Expr.frac_literal_big big:<idx:86>)
  (Expr.int_literal_big)
  (Expr.int_literal_big)
  (Expr.int_literal_big)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 I128)
(var #2 I128)
(var #3 I128)
(var #4 I128)
(var #5 I128)
(var #6 I128)
(var #7 F64)
(var #8 F64)
(var #9 I128)
(var #10 I128)
(var #11 I128)
(var #12 -> #13)
(var #13 tuple)
~~~
# TYPES
~~~roc
~~~
