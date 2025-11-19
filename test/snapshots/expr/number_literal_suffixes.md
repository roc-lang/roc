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
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "u8")
		(e-int (raw "123u8")))
	(field (field "u16")
		(e-int (raw "123u16")))
	(field (field "u32")
		(e-int (raw "123u32")))
	(field (field "u64")
		(e-int (raw "123u64")))
	(field (field "u128")
		(e-int (raw "123u128")))
	(field (field "i8")
		(e-int (raw "123i8")))
	(field (field "i16")
		(e-int (raw "123i16")))
	(field (field "i32")
		(e-int (raw "123i32")))
	(field (field "i64")
		(e-int (raw "123i64")))
	(field (field "i128")
		(e-int (raw "123i128")))
	(field (field "dec")
		(e-int (raw "123dec")))
	(field (field "u8Neg")
		(e-int (raw "-123u8")))
	(field (field "u16Neg")
		(e-int (raw "-123u16")))
	(field (field "u32Neg")
		(e-int (raw "-123u32")))
	(field (field "u64Neg")
		(e-int (raw "-123u64")))
	(field (field "u128Neg")
		(e-int (raw "-123u128")))
	(field (field "i8Neg")
		(e-int (raw "-123i8")))
	(field (field "i16Neg")
		(e-int (raw "-123i16")))
	(field (field "i32Neg")
		(e-int (raw "-123i32")))
	(field (field "i64Neg")
		(e-int (raw "-123i64")))
	(field (field "i128Neg")
		(e-int (raw "-123i128")))
	(field (field "decNeg")
		(e-int (raw "-123dec")))
	(field (field "u8Bin")
		(e-int (raw "0b101u8")))
	(field (field "u16Bin")
		(e-int (raw "0b101u16")))
	(field (field "u32Bin")
		(e-int (raw "0b101u32")))
	(field (field "u64Bin")
		(e-int (raw "0b101u64")))
	(field (field "u128Bin")
		(e-int (raw "0b101u128")))
	(field (field "i8Bin")
		(e-int (raw "0b101i8")))
	(field (field "i16Bin")
		(e-int (raw "0b101i16")))
	(field (field "i32Bin")
		(e-int (raw "0b101i32")))
	(field (field "i64Bin")
		(e-int (raw "0b101i64")))
	(field (field "i128Bin")
		(e-int (raw "0b101i128"))))
~~~
# FORMATTED
~~~roc
{
	u8: 123u8,
	u16: 123u16,
	u32: 123u32,
	u64: 123u64,
	u128: 123u128,
	i8: 123i8,
	i16: 123i16,
	i32: 123i32,
	i64: 123i64,
	i128: 123i128,
	dec: 123dec,
	u8Neg: -123u8,
	u16Neg: -123u16,
	u32Neg: -123u32,
	u64Neg: -123u64,
	u128Neg: -123u128,
	i8Neg: -123i8,
	i16Neg: -123i16,
	i32Neg: -123i32,
	i64Neg: -123i64,
	i128Neg: -123i128,
	decNeg: -123dec,
	u8Bin: 0b101u8,
	u16Bin: 0b101u16,
	u32Bin: 0b101u32,
	u64Bin: 0b101u64,
	u128Bin: 0b101u128,
	i8Bin: 0b101i8,
	i16Bin: 0b101i16,
	i32Bin: 0b101i32,
	i64Bin: 0b101i64,
	i128Bin: 0b101i128,
}
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "u8")
			(e-num (value "123")))
		(field (name "u16")
			(e-num (value "123")))
		(field (name "u32")
			(e-num (value "123")))
		(field (name "u64")
			(e-num (value "123")))
		(field (name "u128")
			(e-num (value "123")))
		(field (name "i8")
			(e-num (value "123")))
		(field (name "i16")
			(e-num (value "123")))
		(field (name "i32")
			(e-num (value "123")))
		(field (name "i64")
			(e-num (value "123")))
		(field (name "i128")
			(e-num (value "123")))
		(field (name "dec")
			(e-num (value "123")))
		(field (name "u8Neg")
			(e-num (value "-123")))
		(field (name "u16Neg")
			(e-num (value "-123")))
		(field (name "u32Neg")
			(e-num (value "-123")))
		(field (name "u64Neg")
			(e-num (value "-123")))
		(field (name "u128Neg")
			(e-num (value "-123")))
		(field (name "i8Neg")
			(e-num (value "-123")))
		(field (name "i16Neg")
			(e-num (value "-123")))
		(field (name "i32Neg")
			(e-num (value "-123")))
		(field (name "i64Neg")
			(e-num (value "-123")))
		(field (name "i128Neg")
			(e-num (value "-123")))
		(field (name "decNeg")
			(e-num (value "-123")))
		(field (name "u8Bin")
			(e-num (value "5")))
		(field (name "u16Bin")
			(e-num (value "5")))
		(field (name "u32Bin")
			(e-num (value "5")))
		(field (name "u64Bin")
			(e-num (value "5")))
		(field (name "u128Bin")
			(e-num (value "5")))
		(field (name "i8Bin")
			(e-num (value "5")))
		(field (name "i16Bin")
			(e-num (value "5")))
		(field (name "i32Bin")
			(e-num (value "5")))
		(field (name "i64Bin")
			(e-num (value "5")))
		(field (name "i128Bin")
			(e-num (value "5")))))
~~~
# TYPES
~~~clojure
(expr (type "{ dec: Num(Frac(Decimal)), decNeg: Num(Frac(Decimal)), i128: Num(Int(Signed128)), i128Bin: Num(Int(Signed128)), i128Neg: Num(Int(Signed128)), i16: Num(Int(Signed16)), i16Bin: Num(Int(Signed16)), i16Neg: Num(Int(Signed16)), i32: Num(Int(Signed32)), i32Bin: Num(Int(Signed32)), i32Neg: Num(Int(Signed32)), i64: Num(Int(Signed64)), i64Bin: Num(Int(Signed64)), i64Neg: Num(Int(Signed64)), i8: Num(Int(Signed8)), i8Bin: Num(Int(Signed8)), i8Neg: Num(Int(Signed8)), u128: Num(Int(Unsigned128)), u128Bin: Num(Int(Unsigned128)), u128Neg: Num(Int(Unsigned128)), u16: Num(Int(Unsigned16)), u16Bin: Num(Int(Unsigned16)), u16Neg: Num(Int(Unsigned16)), u32: Num(Int(Unsigned32)), u32Bin: Num(Int(Unsigned32)), u32Neg: Num(Int(Unsigned32)), u64: Num(Int(Unsigned64)), u64Bin: Num(Int(Unsigned64)), u64Neg: Num(Int(Unsigned64)), u8: Num(Int(Unsigned8)), u8Bin: Num(Int(Unsigned8)), u8Neg: Num(Int(Unsigned8)) }"))
~~~
