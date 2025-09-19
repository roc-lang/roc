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
**INVALID NUMBER**
This number literal is not valid: `123dec`

**number_literal_suffixes.md:12:9:12:15:**
```roc
  dec:  123dec,
```
        ^^^^^^

Check that the number is correctly formatted. Valid examples include: `42`, `3.14`, `0x1A`, or `1_000_000`.

**INVALID NUMBER**
This number literal is not valid: `-123dec`

**number_literal_suffixes.md:23:12:23:19:**
```roc
  decNeg:  -123dec,
```
           ^^^^^^^

Check that the number is correctly formatted. Valid examples include: `42`, `3.14`, `0x1A`, or `1_000_000`.

# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:3-2:5),OpColon(2:5-2:6),Int(2:9-2:14),Comma(2:14-2:15),
LowerIdent(3:3-3:6),OpColon(3:6-3:7),Int(3:9-3:15),Comma(3:15-3:16),
LowerIdent(4:3-4:6),OpColon(4:6-4:7),Int(4:9-4:15),Comma(4:15-4:16),
LowerIdent(5:3-5:6),OpColon(5:6-5:7),Int(5:9-5:15),Comma(5:15-5:16),
LowerIdent(6:3-6:7),OpColon(6:7-6:8),Int(6:9-6:16),Comma(6:16-6:17),
LowerIdent(7:3-7:5),OpColon(7:5-7:6),Int(7:9-7:14),Comma(7:14-7:15),
LowerIdent(8:3-8:6),OpColon(8:6-8:7),Int(8:9-8:15),Comma(8:15-8:16),
LowerIdent(9:3-9:6),OpColon(9:6-9:7),Int(9:9-9:15),Comma(9:15-9:16),
LowerIdent(10:3-10:6),OpColon(10:6-10:7),Int(10:9-10:15),Comma(10:15-10:16),
LowerIdent(11:3-11:7),OpColon(11:7-11:8),Int(11:9-11:16),Comma(11:16-11:17),
LowerIdent(12:3-12:6),OpColon(12:6-12:7),Int(12:9-12:15),Comma(12:15-12:16),
LowerIdent(13:3-13:8),OpColon(13:8-13:9),Int(13:12-13:18),Comma(13:18-13:19),
LowerIdent(14:3-14:9),OpColon(14:9-14:10),Int(14:12-14:19),Comma(14:19-14:20),
LowerIdent(15:3-15:9),OpColon(15:9-15:10),Int(15:12-15:19),Comma(15:19-15:20),
LowerIdent(16:3-16:9),OpColon(16:9-16:10),Int(16:12-16:19),Comma(16:19-16:20),
LowerIdent(17:3-17:10),OpColon(17:10-17:11),Int(17:12-17:20),Comma(17:20-17:21),
LowerIdent(18:3-18:8),OpColon(18:8-18:9),Int(18:12-18:18),Comma(18:18-18:19),
LowerIdent(19:3-19:9),OpColon(19:9-19:10),Int(19:12-19:19),Comma(19:19-19:20),
LowerIdent(20:3-20:9),OpColon(20:9-20:10),Int(20:12-20:19),Comma(20:19-20:20),
LowerIdent(21:3-21:9),OpColon(21:9-21:10),Int(21:12-21:19),Comma(21:19-21:20),
LowerIdent(22:3-22:10),OpColon(22:10-22:11),Int(22:12-22:20),Comma(22:20-22:21),
LowerIdent(23:3-23:9),OpColon(23:9-23:10),Int(23:12-23:19),Comma(23:19-23:20),
LowerIdent(24:3-24:8),OpColon(24:8-24:9),Int(24:12-24:19),Comma(24:19-24:20),
LowerIdent(25:3-25:9),OpColon(25:9-25:10),Int(25:12-25:20),Comma(25:20-25:21),
LowerIdent(26:3-26:9),OpColon(26:9-26:10),Int(26:12-26:20),Comma(26:20-26:21),
LowerIdent(27:3-27:9),OpColon(27:9-27:10),Int(27:12-27:20),Comma(27:20-27:21),
LowerIdent(28:3-28:10),OpColon(28:10-28:11),Int(28:12-28:21),Comma(28:21-28:22),
LowerIdent(29:3-29:8),OpColon(29:8-29:9),Int(29:12-29:19),Comma(29:19-29:20),
LowerIdent(30:3-30:9),OpColon(30:9-30:10),Int(30:12-30:20),Comma(30:20-30:21),
LowerIdent(31:3-31:9),OpColon(31:9-31:10),Int(31:12-31:20),Comma(31:20-31:21),
LowerIdent(32:3-32:9),OpColon(32:9-32:10),Int(32:12-32:20),Comma(32:20-32:21),
LowerIdent(33:3-33:10),OpColon(33:10-33:11),Int(33:12-33:21),Comma(33:21-33:22),
CloseCurly(34:1-34:2),
EndOfFile(35:1-35:1),
~~~
# PARSE
~~~clojure
(e-record @1.1-34.2
	(field (field "u8")
		(e-int @2.9-2.14 (raw "123u8")))
	(field (field "u16")
		(e-int @3.9-3.15 (raw "123u16")))
	(field (field "u32")
		(e-int @4.9-4.15 (raw "123u32")))
	(field (field "u64")
		(e-int @5.9-5.15 (raw "123u64")))
	(field (field "u128")
		(e-int @6.9-6.16 (raw "123u128")))
	(field (field "i8")
		(e-int @7.9-7.14 (raw "123i8")))
	(field (field "i16")
		(e-int @8.9-8.15 (raw "123i16")))
	(field (field "i32")
		(e-int @9.9-9.15 (raw "123i32")))
	(field (field "i64")
		(e-int @10.9-10.15 (raw "123i64")))
	(field (field "i128")
		(e-int @11.9-11.16 (raw "123i128")))
	(field (field "dec")
		(e-int @12.9-12.15 (raw "123dec")))
	(field (field "u8Neg")
		(e-int @13.12-13.18 (raw "-123u8")))
	(field (field "u16Neg")
		(e-int @14.12-14.19 (raw "-123u16")))
	(field (field "u32Neg")
		(e-int @15.12-15.19 (raw "-123u32")))
	(field (field "u64Neg")
		(e-int @16.12-16.19 (raw "-123u64")))
	(field (field "u128Neg")
		(e-int @17.12-17.20 (raw "-123u128")))
	(field (field "i8Neg")
		(e-int @18.12-18.18 (raw "-123i8")))
	(field (field "i16Neg")
		(e-int @19.12-19.19 (raw "-123i16")))
	(field (field "i32Neg")
		(e-int @20.12-20.19 (raw "-123i32")))
	(field (field "i64Neg")
		(e-int @21.12-21.19 (raw "-123i64")))
	(field (field "i128Neg")
		(e-int @22.12-22.20 (raw "-123i128")))
	(field (field "decNeg")
		(e-int @23.12-23.19 (raw "-123dec")))
	(field (field "u8Bin")
		(e-int @24.12-24.19 (raw "0b101u8")))
	(field (field "u16Bin")
		(e-int @25.12-25.20 (raw "0b101u16")))
	(field (field "u32Bin")
		(e-int @26.12-26.20 (raw "0b101u32")))
	(field (field "u64Bin")
		(e-int @27.12-27.20 (raw "0b101u64")))
	(field (field "u128Bin")
		(e-int @28.12-28.21 (raw "0b101u128")))
	(field (field "i8Bin")
		(e-int @29.12-29.19 (raw "0b101i8")))
	(field (field "i16Bin")
		(e-int @30.12-30.20 (raw "0b101i16")))
	(field (field "i32Bin")
		(e-int @31.12-31.20 (raw "0b101i32")))
	(field (field "i64Bin")
		(e-int @32.12-32.20 (raw "0b101i64")))
	(field (field "i128Bin")
		(e-int @33.12-33.21 (raw "0b101i128"))))
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
(e-record @1.1-34.2
	(fields
		(field (name "u8")
			(e-int @2.9-2.14 (value "123") (suffix "u8")))
		(field (name "u16")
			(e-int @3.9-3.15 (value "123") (suffix "u16")))
		(field (name "u32")
			(e-int @4.9-4.15 (value "123") (suffix "u32")))
		(field (name "u64")
			(e-int @5.9-5.15 (value "123") (suffix "u64")))
		(field (name "u128")
			(e-int @6.9-6.16 (value "123") (suffix "u128")))
		(field (name "i8")
			(e-int @7.9-7.14 (value "123") (suffix "i8")))
		(field (name "i16")
			(e-int @8.9-8.15 (value "123") (suffix "i16")))
		(field (name "i32")
			(e-int @9.9-9.15 (value "123") (suffix "i32")))
		(field (name "i64")
			(e-int @10.9-10.15 (value "123") (suffix "i64")))
		(field (name "i128")
			(e-int @11.9-11.16 (value "123") (suffix "i128")))
		(field (name "dec")
			(e-runtime-error (tag "invalid_num_literal")))
		(field (name "u8Neg")
			(e-int @13.12-13.18 (value "-123") (suffix "u8")))
		(field (name "u16Neg")
			(e-int @14.12-14.19 (value "-123") (suffix "u16")))
		(field (name "u32Neg")
			(e-int @15.12-15.19 (value "-123") (suffix "u32")))
		(field (name "u64Neg")
			(e-int @16.12-16.19 (value "-123") (suffix "u64")))
		(field (name "u128Neg")
			(e-int @17.12-17.20 (value "-123") (suffix "u128")))
		(field (name "i8Neg")
			(e-int @18.12-18.18 (value "-123") (suffix "i8")))
		(field (name "i16Neg")
			(e-int @19.12-19.19 (value "-123") (suffix "i16")))
		(field (name "i32Neg")
			(e-int @20.12-20.19 (value "-123") (suffix "i32")))
		(field (name "i64Neg")
			(e-int @21.12-21.19 (value "-123") (suffix "i64")))
		(field (name "i128Neg")
			(e-int @22.12-22.20 (value "-123") (suffix "i128")))
		(field (name "decNeg")
			(e-runtime-error (tag "invalid_num_literal")))
		(field (name "u8Bin")
			(e-int @24.12-24.19 (value "5") (suffix "u8")))
		(field (name "u16Bin")
			(e-int @25.12-25.20 (value "5") (suffix "u16")))
		(field (name "u32Bin")
			(e-int @26.12-26.20 (value "5") (suffix "u32")))
		(field (name "u64Bin")
			(e-int @27.12-27.20 (value "5") (suffix "u64")))
		(field (name "u128Bin")
			(e-int @28.12-28.21 (value "5") (suffix "u128")))
		(field (name "i8Bin")
			(e-int @29.12-29.19 (value "5") (suffix "i8")))
		(field (name "i16Bin")
			(e-int @30.12-30.20 (value "5") (suffix "i16")))
		(field (name "i32Bin")
			(e-int @31.12-31.20 (value "5") (suffix "i32")))
		(field (name "i64Bin")
			(e-int @32.12-32.20 (value "5") (suffix "i64")))
		(field (name "i128Bin")
			(e-int @33.12-33.21 (value "5") (suffix "i128")))))
~~~
# TYPES
~~~clojure
(expr @1.1-34.2 (type "{ u8: Num(Int(Unsigned8)), u16: Num(Int(Unsigned16)), u32: Num(Int(Unsigned32)), u64: Num(Int(Unsigned64)), u128: Num(Int(Unsigned128)), i8: Num(Int(Signed8)), i16: Num(Int(Signed16)), i32: Num(Int(Signed32)), i64: Num(Int(Signed64)), i128: Num(Int(Signed128)), dec: Error, u8Neg: Num(Int(Unsigned8)), u16Neg: Num(Int(Unsigned16)), u32Neg: Num(Int(Unsigned32)), u64Neg: Num(Int(Unsigned64)), u128Neg: Num(Int(Unsigned128)), i8Neg: Num(Int(Signed8)), i16Neg: Num(Int(Signed16)), i32Neg: Num(Int(Signed32)), i64Neg: Num(Int(Signed64)), i128Neg: Num(Int(Signed128)), decNeg: Error, u8Bin: Num(Int(Unsigned8)), u16Bin: Num(Int(Unsigned16)), u32Bin: Num(Int(Unsigned32)), u64Bin: Num(Int(Unsigned64)), u128Bin: Num(Int(Unsigned128)), i8Bin: Num(Int(Signed8)), i16Bin: Num(Int(Signed16)), i32Bin: Num(Int(Signed32)), i64Bin: Num(Int(Signed64)), i128Bin: Num(Int(Signed128)) }"))
~~~
