# META
~~~ini
description=Tuple containing various supported number formats
type=expr
~~~
# SOURCE
~~~roc
(1u8, 2i8, 3u16, 4i16, 5u32, 6i32, 7u64, 8i64, 9u128, 10i128, 11.0f32, 12.0f64, 13.0dec, 0xE, 0xf, 0x20, 0b10001, 0b1_0010, 19, 20.0, 21_000, 22_000_000, 0.0, -0.1, 2e4, 3E2, -0.2e-2)
~~~
# TOKENS
~~~text
OpenRound Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Float LowerIdent Comma Float LowerIdent Comma Float LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int Comma Float Comma Int Underscore Int Comma Int Underscore Int Underscore Int Comma Float Comma OpUnaryMinus Float Comma Float Comma Float Comma OpUnaryMinus Float CloseRound ~~~
# PARSE
~~~clojure
(num_literal_i32 1)
~~~
# FORMATTED
~~~roc
1
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:3 to 1:3

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
