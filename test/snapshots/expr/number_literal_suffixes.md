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
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:2:9:2:14
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:3:9:3:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:4:9:4:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:5:9:5:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:6:9:6:16
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:7:9:7:14
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:8:9:8:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:9:9:9:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:10:9:10:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:11:9:11:16
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:12:9:12:15
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:13:12:13:18
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:14:12:14:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:15:12:15:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:16:12:16:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:17:12:17:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:18:12:18:18
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:19:12:19:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:20:12:20:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:21:12:21:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:22:12:22:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:23:12:23:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:24:12:24:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:25:12:25:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:26:12:26:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:27:12:27:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:28:12:28:21
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:29:12:29:19
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:30:12:30:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:31:12:31:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:32:12:32:20
DEPRECATED NUMBER SUFFIX - number_literal_suffixes.md:33:12:33:21
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:2:9:2:14:**
```roc
  u8:   123u8,
```
        ^^^^^

The `u8` suffix is no longer supported. Use `123.U8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:3:9:3:15:**
```roc
  u16:  123u16,
```
        ^^^^^^

The `u16` suffix is no longer supported. Use `123.U16` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:4:9:4:15:**
```roc
  u32:  123u32,
```
        ^^^^^^

The `u32` suffix is no longer supported. Use `123.U32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:5:9:5:15:**
```roc
  u64:  123u64,
```
        ^^^^^^

The `u64` suffix is no longer supported. Use `123.U64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:6:9:6:16:**
```roc
  u128: 123u128,
```
        ^^^^^^^

The `u128` suffix is no longer supported. Use `123.U128` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:7:9:7:14:**
```roc
  i8:   123i8,
```
        ^^^^^

The `i8` suffix is no longer supported. Use `123.I8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:8:9:8:15:**
```roc
  i16:  123i16,
```
        ^^^^^^

The `i16` suffix is no longer supported. Use `123.I16` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:9:9:9:15:**
```roc
  i32:  123i32,
```
        ^^^^^^

The `i32` suffix is no longer supported. Use `123.I32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:10:9:10:15:**
```roc
  i64:  123i64,
```
        ^^^^^^

The `i64` suffix is no longer supported. Use `123.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:11:9:11:16:**
```roc
  i128: 123i128,
```
        ^^^^^^^

The `i128` suffix is no longer supported. Use `123.I128` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:12:9:12:15:**
```roc
  dec:  123dec,
```
        ^^^^^^

The `dec` suffix is no longer supported. Use `123.Dec` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:13:12:13:18:**
```roc
  u8Neg:   -123u8,
```
           ^^^^^^

The `u8` suffix is no longer supported. Use `-123.U8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:14:12:14:19:**
```roc
  u16Neg:  -123u16,
```
           ^^^^^^^

The `u16` suffix is no longer supported. Use `-123.U16` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:15:12:15:19:**
```roc
  u32Neg:  -123u32,
```
           ^^^^^^^

The `u32` suffix is no longer supported. Use `-123.U32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:16:12:16:19:**
```roc
  u64Neg:  -123u64,
```
           ^^^^^^^

The `u64` suffix is no longer supported. Use `-123.U64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:17:12:17:20:**
```roc
  u128Neg: -123u128,
```
           ^^^^^^^^

The `u128` suffix is no longer supported. Use `-123.U128` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:18:12:18:18:**
```roc
  i8Neg:   -123i8,
```
           ^^^^^^

The `i8` suffix is no longer supported. Use `-123.I8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:19:12:19:19:**
```roc
  i16Neg:  -123i16,
```
           ^^^^^^^

The `i16` suffix is no longer supported. Use `-123.I16` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:20:12:20:19:**
```roc
  i32Neg:  -123i32,
```
           ^^^^^^^

The `i32` suffix is no longer supported. Use `-123.I32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:21:12:21:19:**
```roc
  i64Neg:  -123i64,
```
           ^^^^^^^

The `i64` suffix is no longer supported. Use `-123.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:22:12:22:20:**
```roc
  i128Neg: -123i128,
```
           ^^^^^^^^

The `i128` suffix is no longer supported. Use `-123.I128` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:23:12:23:19:**
```roc
  decNeg:  -123dec,
```
           ^^^^^^^

The `dec` suffix is no longer supported. Use `-123.Dec` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:24:12:24:19:**
```roc
  u8Bin:   0b101u8,
```
           ^^^^^^^

The `u8` suffix is no longer supported. Use `0b101.U8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:25:12:25:20:**
```roc
  u16Bin:  0b101u16,
```
           ^^^^^^^^

The `u16` suffix is no longer supported. Use `0b101.U16` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:26:12:26:20:**
```roc
  u32Bin:  0b101u32,
```
           ^^^^^^^^

The `u32` suffix is no longer supported. Use `0b101.U32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:27:12:27:20:**
```roc
  u64Bin:  0b101u64,
```
           ^^^^^^^^

The `u64` suffix is no longer supported. Use `0b101.U64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:28:12:28:21:**
```roc
  u128Bin: 0b101u128,
```
           ^^^^^^^^^

The `u128` suffix is no longer supported. Use `0b101.U128` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:29:12:29:19:**
```roc
  i8Bin:   0b101i8,
```
           ^^^^^^^

The `i8` suffix is no longer supported. Use `0b101.I8` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:30:12:30:20:**
```roc
  i16Bin:  0b101i16,
```
           ^^^^^^^^

The `i16` suffix is no longer supported. Use `0b101.I16` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:31:12:31:20:**
```roc
  i32Bin:  0b101i32,
```
           ^^^^^^^^

The `i32` suffix is no longer supported. Use `0b101.I32` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:32:12:32:20:**
```roc
  i64Bin:  0b101i64,
```
           ^^^^^^^^

The `i64` suffix is no longer supported. Use `0b101.I64` instead.

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**number_literal_suffixes.md:33:12:33:21:**
```roc
  i128Bin: 0b101i128,
```
           ^^^^^^^^^

The `i128` suffix is no longer supported. Use `0b101.I128` instead.

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
(expr (type "{ dec: Dec, decNeg: Dec, i128: I128, i128Bin: I128, i128Neg: I128, i16: I16, i16Bin: I16, i16Neg: I16, i32: I32, i32Bin: I32, i32Neg: I32, i64: I64, i64Bin: I64, i64Neg: I64, i8: I8, i8Bin: I8, i8Neg: I8, u128: U128, u128Bin: U128, u128Neg: U128, u16: U16, u16Bin: U16, u16Neg: U16, u32: U32, u32Bin: U32, u32Neg: U32, u64: U64, u64Bin: U64, u64Neg: U64, u8: U8, u8Bin: U8, u8Neg: U8 }"))
~~~
