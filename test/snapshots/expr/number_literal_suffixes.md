# META
~~~ini
description=number_literal_suffixes
type=expr
~~~
# SOURCE
~~~roc
{
  u8:   123.U8,
  u16:  123.U16,
  u32:  123.U32,
  u64:  123.U64,
  u128: 123.U128,
  i8:   123.I8,
  i16:  123.I16,
  i32:  123.I32,
  i64:  123.I64,
  i128: 123.I128,
  dec:  123.Dec,
  u8Neg:   -123.U8,
  u16Neg:  -123.U16,
  u32Neg:  -123.U32,
  u64Neg:  -123.U64,
  u128Neg: -123.U128,
  i8Neg:   -123.I8,
  i16Neg:  -123.I16,
  i32Neg:  -123.I32,
  i64Neg:  -123.I64,
  i128Neg: -123.I128,
  decNeg:  -123.Dec,
  u8Bin:   0b101.U8,
  u16Bin:  0b101.U16,
  u32Bin:  0b101.U32,
  u64Bin:  0b101.U64,
  u128Bin: 0b101.U128,
  i8Bin:   0b101.I8,
  i16Bin:  0b101.I16,
  i32Bin:  0b101.I32,
  i64Bin:  0b101.I64,
  i128Bin: 0b101.I128,
}
~~~
# EXPECTED
INVALID NUMBER - number_literal_suffixes.md:13:12:13:19
INVALID NUMBER - number_literal_suffixes.md:14:12:14:20
INVALID NUMBER - number_literal_suffixes.md:15:12:15:20
INVALID NUMBER - number_literal_suffixes.md:16:12:16:20
INVALID NUMBER - number_literal_suffixes.md:17:12:17:21
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  u8Neg:   -123.U8,                                                         │
 │           ‾‾‾‾‾‾‾                                                          │
 └────────────────────────────────────────── number_literal_suffixes.md:13:12 ┘

    The inferred type is:

        U8


┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  u16Neg:  -123.U16,                                                        │
 │           ‾‾‾‾‾‾‾‾                                                         │
 └────────────────────────────────────────── number_literal_suffixes.md:14:12 ┘

    The inferred type is:

        U16


┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  u32Neg:  -123.U32,                                                        │
 │           ‾‾‾‾‾‾‾‾                                                         │
 └────────────────────────────────────────── number_literal_suffixes.md:15:12 ┘

    The inferred type is:

        U32


┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  u64Neg:  -123.U64,                                                        │
 │           ‾‾‾‾‾‾‾‾                                                         │
 └────────────────────────────────────────── number_literal_suffixes.md:16:12 ┘

    The inferred type is:

        U64


┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  u128Neg: -123.U128,                                                       │
 │           ‾‾‾‾‾‾‾‾‾                                                        │
 └────────────────────────────────────────── number_literal_suffixes.md:17:12 ┘

    The inferred type is:

        U128

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "u8")
		(e-typed-int (raw "123") (type "U8")))
	(field (field "u16")
		(e-typed-int (raw "123") (type "U16")))
	(field (field "u32")
		(e-typed-int (raw "123") (type "U32")))
	(field (field "u64")
		(e-typed-int (raw "123") (type "U64")))
	(field (field "u128")
		(e-typed-int (raw "123") (type "U128")))
	(field (field "i8")
		(e-typed-int (raw "123") (type "I8")))
	(field (field "i16")
		(e-typed-int (raw "123") (type "I16")))
	(field (field "i32")
		(e-typed-int (raw "123") (type "I32")))
	(field (field "i64")
		(e-typed-int (raw "123") (type "I64")))
	(field (field "i128")
		(e-typed-int (raw "123") (type "I128")))
	(field (field "dec")
		(e-typed-int (raw "123") (type "Dec")))
	(field (field "u8Neg")
		(e-typed-int (raw "-123") (type "U8")))
	(field (field "u16Neg")
		(e-typed-int (raw "-123") (type "U16")))
	(field (field "u32Neg")
		(e-typed-int (raw "-123") (type "U32")))
	(field (field "u64Neg")
		(e-typed-int (raw "-123") (type "U64")))
	(field (field "u128Neg")
		(e-typed-int (raw "-123") (type "U128")))
	(field (field "i8Neg")
		(e-typed-int (raw "-123") (type "I8")))
	(field (field "i16Neg")
		(e-typed-int (raw "-123") (type "I16")))
	(field (field "i32Neg")
		(e-typed-int (raw "-123") (type "I32")))
	(field (field "i64Neg")
		(e-typed-int (raw "-123") (type "I64")))
	(field (field "i128Neg")
		(e-typed-int (raw "-123") (type "I128")))
	(field (field "decNeg")
		(e-typed-int (raw "-123") (type "Dec")))
	(field (field "u8Bin")
		(e-typed-int (raw "0b101") (type "U8")))
	(field (field "u16Bin")
		(e-typed-int (raw "0b101") (type "U16")))
	(field (field "u32Bin")
		(e-typed-int (raw "0b101") (type "U32")))
	(field (field "u64Bin")
		(e-typed-int (raw "0b101") (type "U64")))
	(field (field "u128Bin")
		(e-typed-int (raw "0b101") (type "U128")))
	(field (field "i8Bin")
		(e-typed-int (raw "0b101") (type "I8")))
	(field (field "i16Bin")
		(e-typed-int (raw "0b101") (type "I16")))
	(field (field "i32Bin")
		(e-typed-int (raw "0b101") (type "I32")))
	(field (field "i64Bin")
		(e-typed-int (raw "0b101") (type "I64")))
	(field (field "i128Bin")
		(e-typed-int (raw "0b101") (type "I128"))))
~~~
# FORMATTED
~~~roc
{
	u8: 123.U8,
	u16: 123.U16,
	u32: 123.U32,
	u64: 123.U64,
	u128: 123.U128,
	i8: 123.I8,
	i16: 123.I16,
	i32: 123.I32,
	i64: 123.I64,
	i128: 123.I128,
	dec: 123.Dec,
	u8Neg: -123.U8,
	u16Neg: -123.U16,
	u32Neg: -123.U32,
	u64Neg: -123.U64,
	u128Neg: -123.U128,
	i8Neg: -123.I8,
	i16Neg: -123.I16,
	i32Neg: -123.I32,
	i64Neg: -123.I64,
	i128Neg: -123.I128,
	decNeg: -123.Dec,
	u8Bin: 0b101.U8,
	u16Bin: 0b101.U16,
	u32Bin: 0b101.U32,
	u64Bin: 0b101.U64,
	u128Bin: 0b101.U128,
	i8Bin: 0b101.I8,
	i16Bin: 0b101.I16,
	i32Bin: 0b101.I32,
	i64Bin: 0b101.I64,
	i128Bin: 0b101.I128,
}
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "u8")
			(e-typed-int (value "123") (type "U8")))
		(field (name "u16")
			(e-typed-int (value "123") (type "U16")))
		(field (name "u32")
			(e-typed-int (value "123") (type "U32")))
		(field (name "u64")
			(e-typed-int (value "123") (type "U64")))
		(field (name "u128")
			(e-typed-int (value "123") (type "U128")))
		(field (name "i8")
			(e-typed-int (value "123") (type "I8")))
		(field (name "i16")
			(e-typed-int (value "123") (type "I16")))
		(field (name "i32")
			(e-typed-int (value "123") (type "I32")))
		(field (name "i64")
			(e-typed-int (value "123") (type "I64")))
		(field (name "i128")
			(e-typed-int (value "123") (type "I128")))
		(field (name "dec")
			(e-typed-int (value "123") (type "Dec")))
		(field (name "u8Neg")
			(e-typed-int (value "-123") (type "U8")))
		(field (name "u16Neg")
			(e-typed-int (value "-123") (type "U16")))
		(field (name "u32Neg")
			(e-typed-int (value "-123") (type "U32")))
		(field (name "u64Neg")
			(e-typed-int (value "-123") (type "U64")))
		(field (name "u128Neg")
			(e-typed-int (value "-123") (type "U128")))
		(field (name "i8Neg")
			(e-typed-int (value "-123") (type "I8")))
		(field (name "i16Neg")
			(e-typed-int (value "-123") (type "I16")))
		(field (name "i32Neg")
			(e-typed-int (value "-123") (type "I32")))
		(field (name "i64Neg")
			(e-typed-int (value "-123") (type "I64")))
		(field (name "i128Neg")
			(e-typed-int (value "-123") (type "I128")))
		(field (name "decNeg")
			(e-typed-int (value "-123") (type "Dec")))
		(field (name "u8Bin")
			(e-typed-int (value "5") (type "U8")))
		(field (name "u16Bin")
			(e-typed-int (value "5") (type "U16")))
		(field (name "u32Bin")
			(e-typed-int (value "5") (type "U32")))
		(field (name "u64Bin")
			(e-typed-int (value "5") (type "U64")))
		(field (name "u128Bin")
			(e-typed-int (value "5") (type "U128")))
		(field (name "i8Bin")
			(e-typed-int (value "5") (type "I8")))
		(field (name "i16Bin")
			(e-typed-int (value "5") (type "I16")))
		(field (name "i32Bin")
			(e-typed-int (value "5") (type "I32")))
		(field (name "i64Bin")
			(e-typed-int (value "5") (type "I64")))
		(field (name "i128Bin")
			(e-typed-int (value "5") (type "I128")))))
~~~
# TYPES
~~~clojure
(expr (type "{ dec: Dec, decNeg: Dec, i128: I128, i128Bin: I128, i128Neg: I128, i16: I16, i16Bin: I16, i16Neg: I16, i32: I32, i32Bin: I32, i32Neg: I32, i64: I64, i64Bin: I64, i64Neg: I64, i8: I8, i8Bin: I8, i8Neg: I8, u128: U128, u128Bin: U128, u128Neg: Error, u16: U16, u16Bin: U16, u16Neg: Error, u32: U32, u32Bin: U32, u32Neg: Error, u64: U64, u64Bin: U64, u64Neg: Error, u8: U8, u8Bin: U8, u8Neg: Error }"))
~~~
