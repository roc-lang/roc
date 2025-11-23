# META
~~~ini
description=Tuple containing various supported number formats
type=expr
~~~
# SOURCE
~~~roc
(1u8, 2i8, 3u16, 4i16, 5u32, 6i32, 7u64, 8i64, 9u128, 10i128, 11.0f32, 12.0f64, 13.0dec, 0xE, 0xf, 0x20, 0b10001, 0b1_0010, 19, 20.0, 21_000, 22_000_000, 0.0, -0.1, 2e4, 3E2, -0.2e-2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Float,Comma,Float,Comma,Float,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Float,Comma,Int,Comma,Int,Comma,Float,Comma,Float,Comma,Float,Comma,Float,Comma,Float,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-int (raw "1u8"))
	(e-int (raw "2i8"))
	(e-int (raw "3u16"))
	(e-int (raw "4i16"))
	(e-int (raw "5u32"))
	(e-int (raw "6i32"))
	(e-int (raw "7u64"))
	(e-int (raw "8i64"))
	(e-int (raw "9u128"))
	(e-int (raw "10i128"))
	(e-frac (raw "11.0f32"))
	(e-frac (raw "12.0f64"))
	(e-frac (raw "13.0dec"))
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
		(e-num (value "1"))
		(e-num (value "2"))
		(e-num (value "3"))
		(e-num (value "4"))
		(e-num (value "5"))
		(e-num (value "6"))
		(e-num (value "7"))
		(e-num (value "8"))
		(e-num (value "9"))
		(e-num (value "10"))
		(e-frac-f32 (value "11"))
		(e-frac-f64 (value "12"))
		(e-frac-dec (value "13"))
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
(expr (type "(U8, I8, U16, I16, U32, I32, U64, I64, U128, I128, F32, F64, Dec, a, b, c, d, e, f, g, h, i, j, k, l, m, n) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]), g.from_numeral : Numeral -> Try(g, [InvalidNumeral(Str)]), h.from_numeral : Numeral -> Try(h, [InvalidNumeral(Str)]), i.from_numeral : Numeral -> Try(i, [InvalidNumeral(Str)]), j.from_numeral : Numeral -> Try(j, [InvalidNumeral(Str)]), k.from_numeral : Numeral -> Try(k, [InvalidNumeral(Str)]), l.from_numeral : Numeral -> Try(l, [InvalidNumeral(Str)]), m.from_numeral : Numeral -> Try(m, [InvalidNumeral(Str)]), n.from_numeral : Numeral -> Try(n, [InvalidNumeral(Str)])]"))
~~~
