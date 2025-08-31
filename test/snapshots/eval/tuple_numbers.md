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
OpenRound Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Int LowerIdent Comma Float LowerIdent Comma Float LowerIdent Comma Float LowerIdent Comma Int Comma Int Comma Int Comma Int Comma Int Comma Int Comma Float Comma Int Underscore Int Comma Int Underscore Int Underscore Int Comma Float Comma OpUnaryMinus Float Comma Float Comma Float Comma OpUnaryMinus Float CloseRound ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**tuple_numbers.md:1:3:1:5:**
```roc
(1u8, 2i8, 3u16, 4i16, 5u32, 6i32, 7u64, 8i64, 9u128, 10i128, 11.0f32, 12.0f64, 13.0dec, 0xE, 0xf, 0x20, 0b10001, 0b1_0010, 19, 20.0, 21_000, 22_000_000, 0.0, -0.1, 2e4, 3E2, -0.2e-2)
```
  ^^


# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
