# META
~~~ini
description=Tuple containing various supported number formats
type=expr
~~~
# SOURCE
~~~roc
(1.U8, 2.I8, 3.U16, 4.I16, 5.U32, 6.I32, 7.U64, 8.I64, 9.U128, 10.I128, 11.0.F32, 12.0.F64, 13.0.Dec, 0xE, 0xf, 0x20, 0b10001, 0b1_0010, 19, 20.0, 21_000, 22_000_000, 0.0, -0.1, 2e4, 3E2, -0.2e-2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Float,NoSpaceDotUpperIdent,Comma,Float,NoSpaceDotUpperIdent,Comma,Float,NoSpaceDotUpperIdent,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Float,Comma,Int,Comma,Int,Comma,Float,Comma,Float,Comma,Float,Comma,Float,Comma,Float,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-typed-int (raw "1") (type ".U8"))
	(e-typed-int (raw "2") (type ".I8"))
	(e-typed-int (raw "3") (type ".U16"))
	(e-typed-int (raw "4") (type ".I16"))
	(e-typed-int (raw "5") (type ".U32"))
	(e-typed-int (raw "6") (type ".I32"))
	(e-typed-int (raw "7") (type ".U64"))
	(e-typed-int (raw "8") (type ".I64"))
	(e-typed-int (raw "9") (type ".U128"))
	(e-typed-int (raw "10") (type ".I128"))
	(e-typed-frac (raw "11.0") (type ".F32"))
	(e-typed-frac (raw "12.0") (type ".F64"))
	(e-typed-frac (raw "13.0") (type ".Dec"))
	(e-int (raw "0xE"))
	(e-int (raw "0xf"))
	(e-int (raw "0x20"))
	(e-int (raw "0b10001"))
	(e-int (raw "0b1_0010"))
	(e-int (raw "19"))
	(e-frac (raw "20.0"))
	(e-int (raw "21_000"))
	(e-int (raw "22_000_000"))
	(e-frac (raw "0.0"))
	(e-frac (raw "-0.1"))
	(e-frac (raw "2e4"))
	(e-frac (raw "3E2"))
	(e-frac (raw "-0.2e-2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-typed-int (value "1") (type "U8"))
		(e-typed-int (value "2") (type "I8"))
		(e-typed-int (value "3") (type "U16"))
		(e-typed-int (value "4") (type "I16"))
		(e-typed-int (value "5") (type "U32"))
		(e-typed-int (value "6") (type "I32"))
		(e-typed-int (value "7") (type "U64"))
		(e-typed-int (value "8") (type "I64"))
		(e-typed-int (value "9") (type "U128"))
		(e-typed-int (value "10") (type "I128"))
		(e-typed-frac (value "11000000000000000000") (type "F32"))
		(e-typed-frac (value "12000000000000000000") (type "F64"))
		(e-typed-frac (value "13000000000000000000") (type "Dec"))
		(e-num (value "14"))
		(e-num (value "15"))
		(e-num (value "32"))
		(e-num (value "17"))
		(e-num (value "18"))
		(e-num (value "19"))
		(e-dec-small (numerator "200") (denominator-power-of-ten "1") (value "20"))
		(e-num (value "21000"))
		(e-num (value "22000000"))
		(e-dec-small (numerator "0") (denominator-power-of-ten "1") (value "0.0"))
		(e-dec-small (numerator "-1") (denominator-power-of-ten "1") (value "-0.1"))
		(e-dec-small (numerator "20000") (denominator-power-of-ten "0") (value "20000"))
		(e-dec-small (numerator "300") (denominator-power-of-ten "0") (value "300"))
		(e-frac-dec (value "-0.002"))))
~~~
# TYPES
~~~clojure
(expr (type "(U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, F32, F64, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec, Dec)"))
~~~
