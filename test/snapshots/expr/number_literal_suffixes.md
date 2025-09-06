# META
~~~ini
description=number_literal_suffixes
type=expr
~~~
# SOURCE
~~~roc
{
  u8:   123u8,
  u16:  123u16,
  u32:  123u32,
  u64:  123u64,
  u128: 123u128,
  i8:   123i8,
  i16:  123i16,
  i32:  123i32,
  i64:  123i64,
  i128: 123i128,
  dec:  123dec,
  u8Neg:   -123u8,
  u16Neg:  -123u16,
  u32Neg:  -123u32,
  u64Neg:  -123u64,
  u128Neg: -123u128,
  i8Neg:   -123i8,
  i16Neg:  -123i16,
  i32Neg:  -123i32,
  i64Neg:  -123i64,
  i128Neg: -123i128,
  decNeg:  -123dec,
  u8Bin:   0b101u8,
  u16Bin:  0b101u16,
  u32Bin:  0b101u32,
  u64Bin:  0b101u64,
  u128Bin: 0b101u128,
  i8Bin:   0b101i8,
  i16Bin:  0b101i16,
  i32Bin:  0b101i32,
  i64Bin:  0b101i64,
  i128Bin: 0b101i128,
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon OpUnaryMinus Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma LowerIdent OpColon Int LowerIdent Comma CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
u16
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**number_literal_suffixes.md:1:1:3:12:**
```roc
{
  u8:   123u8,
  u16:  123u16,
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **u16** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**number_literal_suffixes.md:3:12:3:15:**
```roc
  u16:  123u16,
```
           ^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
~~~
