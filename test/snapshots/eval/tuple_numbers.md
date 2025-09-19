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
OpenRound(1:1-1:2),Int(1:2-1:5),Comma(1:5-1:6),Int(1:7-1:10),Comma(1:10-1:11),Int(1:12-1:16),Comma(1:16-1:17),Int(1:18-1:22),Comma(1:22-1:23),Int(1:24-1:28),Comma(1:28-1:29),Int(1:30-1:34),Comma(1:34-1:35),Int(1:36-1:40),Comma(1:40-1:41),Int(1:42-1:46),Comma(1:46-1:47),Int(1:48-1:53),Comma(1:53-1:54),Int(1:55-1:61),Comma(1:61-1:62),Float(1:63-1:70),Comma(1:70-1:71),Float(1:72-1:79),Comma(1:79-1:80),Float(1:81-1:88),Comma(1:88-1:89),Int(1:90-1:93),Comma(1:93-1:94),Int(1:95-1:98),Comma(1:98-1:99),Int(1:100-1:104),Comma(1:104-1:105),Int(1:106-1:113),Comma(1:113-1:114),Int(1:115-1:123),Comma(1:123-1:124),Int(1:125-1:127),Comma(1:127-1:128),Float(1:129-1:133),Comma(1:133-1:134),Int(1:135-1:141),Comma(1:141-1:142),Int(1:143-1:153),Comma(1:153-1:154),Float(1:155-1:158),Comma(1:158-1:159),Float(1:160-1:164),Comma(1:164-1:165),Float(1:166-1:169),Comma(1:169-1:170),Float(1:171-1:174),Comma(1:174-1:175),Float(1:176-1:183),CloseRound(1:183-1:184),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.184
	(e-int @1.2-1.5 (raw "1u8"))
	(e-int @1.7-1.10 (raw "2i8"))
	(e-int @1.12-1.16 (raw "3u16"))
	(e-int @1.18-1.22 (raw "4i16"))
	(e-int @1.24-1.28 (raw "5u32"))
	(e-int @1.30-1.34 (raw "6i32"))
	(e-int @1.36-1.40 (raw "7u64"))
	(e-int @1.42-1.46 (raw "8i64"))
	(e-int @1.48-1.53 (raw "9u128"))
	(e-int @1.55-1.61 (raw "10i128"))
	(e-frac @1.63-1.70 (raw "11.0f32"))
	(e-frac @1.72-1.79 (raw "12.0f64"))
	(e-frac @1.81-1.88 (raw "13.0dec"))
	(e-int @1.90-1.93 (raw "0xE"))
	(e-int @1.95-1.98 (raw "0xf"))
	(e-int @1.100-1.104 (raw "0x20"))
	(e-int @1.106-1.113 (raw "0b10001"))
	(e-int @1.115-1.123 (raw "0b1_0010"))
	(e-int @1.125-1.127 (raw "19"))
	(e-frac @1.129-1.133 (raw "20.0"))
	(e-int @1.135-1.141 (raw "21_000"))
	(e-int @1.143-1.153 (raw "22_000_000"))
	(e-frac @1.155-1.158 (raw "0.0"))
	(e-frac @1.160-1.164 (raw "-0.1"))
	(e-frac @1.166-1.169 (raw "2e4"))
	(e-frac @1.171-1.174 (raw "3E2"))
	(e-frac @1.176-1.183 (raw "-0.2e-2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.184
	(elems
		(e-int @1.2-1.5 (value "1") (suffix "u8"))
		(e-int @1.7-1.10 (value "2") (suffix "i8"))
		(e-int @1.12-1.16 (value "3") (suffix "u16"))
		(e-int @1.18-1.22 (value "4") (suffix "i16"))
		(e-int @1.24-1.28 (value "5") (suffix "u32"))
		(e-int @1.30-1.34 (value "6") (suffix "i32"))
		(e-int @1.36-1.40 (value "7") (suffix "u64"))
		(e-int @1.42-1.46 (value "8") (suffix "i64"))
		(e-int @1.48-1.53 (value "9") (suffix "u128"))
		(e-int @1.55-1.61 (value "10") (suffix "i128"))
		(e-frac-f32 @1.63-1.70 (value "11"))
		(e-frac-f64 @1.72-1.79 (value "12"))
		(e-frac-dec @1.81-1.88 (value "13"))
		(e-int @1.90-1.93 (value "14") (suffix "none"))
		(e-int @1.95-1.98 (value "15") (suffix "none"))
		(e-int @1.100-1.104 (value "32") (suffix "none"))
		(e-int @1.106-1.113 (value "17") (suffix "none"))
		(e-int @1.115-1.123 (value "18") (suffix "none"))
		(e-num @1.125-1.127 (value "19"))
		(e-dec-small @1.129-1.133 (numerator "200") (denominator-power-of-ten "1") (value "20"))
		(e-num @1.135-1.141 (value "21000"))
		(e-num @1.143-1.153 (value "22000000"))
		(e-dec-small @1.155-1.158 (numerator "0") (denominator-power-of-ten "1") (value "0.0"))
		(e-dec-small @1.160-1.164 (numerator "-1") (denominator-power-of-ten "1") (value "-0.1"))
		(e-dec-small @1.166-1.169 (numerator "20000") (denominator-power-of-ten "0") (value "20000"))
		(e-dec-small @1.171-1.174 (numerator "300") (denominator-power-of-ten "0") (value "300"))
		(e-frac-dec @1.176-1.183 (value "-0.002"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.184 (type "(Num(Int(Unsigned8)), Num(Int(Signed8)), Num(Int(Unsigned16)), Num(Int(Signed16)), Num(Int(Unsigned32)), Num(Int(Signed32)), Num(Int(Unsigned64)), Num(Int(Signed64)), Num(Int(Unsigned128)), Num(Int(Signed128)), Num(Frac(Float32)), Num(Frac(Float64)), Num(Frac(Decimal)), Num(Int(_size)), Num(Int(_size2)), Num(Int(_size3)), Num(Int(_size4)), Num(Int(_size5)), Num(_size6), Num(Frac(_size7)), Num(_size8), Num(_size9), Num(Frac(_size10)), Num(Frac(_size11)), Num(Frac(_size12)), Num(Frac(_size13)), Num(Frac(_size14)))"))
~~~
