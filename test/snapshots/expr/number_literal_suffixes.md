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
(record_literal
  (binop_colon
    (lc "u8")
    (num_literal_i32 123)
  )
  (lc "u8")
  (binop_colon
    (lc "u16")
    (num_literal_i32 123)
  )
)
~~~
# FORMATTED
~~~roc
{ u8 : 123, u8, u16 : 123 }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 3:12

# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
