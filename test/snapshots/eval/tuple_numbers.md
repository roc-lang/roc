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
OpenRound Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Float LowerIdent Comma Float LowerIdent Comma Float LowerIdent Comma IntBase Comma IntBase Comma IntBase Comma IntBase Comma IntBase Comma Int Comma Float Comma Int Underscore Int Comma Int Underscore Int Underscore Int Comma Float Comma OpUnaryMinus Float Comma Float Comma Float Comma OpUnaryMinus Float CloseRound ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
u8
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **u8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**tuple_numbers.md:1:3:1:5:**
```roc
(1u8, 2i8, 3u16, 4i16, 5u32, 6i32, 7u64, 8i64, 9u128, 10i128, 11.0f32, 12.0f64, 13.0dec, 0xE, 0xf, 0x20, 0b10001, 0b1_0010, 19, 20.0, 21_000, 22_000_000, 0.0, -0.1, 2e4, 3E2, -0.2e-2)
```
  ^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
