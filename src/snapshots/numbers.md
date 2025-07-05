# META
~~~ini
description=Number formats
type=expr
~~~
# SOURCE
~~~roc
(
    0X42,
    0x42,
    0B01,
    0b01,
    0O42,
    0o42,
    0.1e42,
    0.1E42,
    0xDEADBEEF,
    0xdeadbeef,
    0xDeAdBeEf,
)
~~~
# EXPECTED
NIL
# PROBLEMS
**UPPERCASE BASE**
Number base prefixes must be lowercase (0x, 0o, 0b).

**UPPERCASE BASE**
Number base prefixes must be lowercase (0x, 0o, 0b).

**UPPERCASE BASE**
Number base prefixes must be lowercase (0x, 0o, 0b).

# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
Int(2:5-2:9),Comma(2:9-2:10),Newline(1:1-1:1),
Int(3:5-3:9),Comma(3:9-3:10),Newline(1:1-1:1),
Int(4:5-4:9),Comma(4:9-4:10),Newline(1:1-1:1),
Int(5:5-5:9),Comma(5:9-5:10),Newline(1:1-1:1),
Int(6:5-6:9),Comma(6:9-6:10),Newline(1:1-1:1),
Int(7:5-7:9),Comma(7:9-7:10),Newline(1:1-1:1),
Float(8:5-8:11),Comma(8:11-8:12),Newline(1:1-1:1),
Float(9:5-9:11),Comma(9:11-9:12),Newline(1:1-1:1),
Int(10:5-10:15),Comma(10:15-10:16),Newline(1:1-1:1),
Int(11:5-11:15),Comma(11:15-11:16),Newline(1:1-1:1),
Int(12:5-12:15),Comma(12:15-12:16),Newline(1:1-1:1),
CloseRound(13:1-13:2),EndOfFile(13:2-13:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-13.2
	(e-int @2.5-2.9 (raw "0X42"))
	(e-int @3.5-3.9 (raw "0x42"))
	(e-int @4.5-4.9 (raw "0B01"))
	(e-int @5.5-5.9 (raw "0b01"))
	(e-int @6.5-6.9 (raw "0O42"))
	(e-int @7.5-7.9 (raw "0o42"))
	(e-frac @8.5-8.11 (raw "0.1e42"))
	(e-frac @9.5-9.11 (raw "0.1E42"))
	(e-int @10.5-10.15 (raw "0xDEADBEEF"))
	(e-int @11.5-11.15 (raw "0xdeadbeef"))
	(e-int @12.5-12.15 (raw "0xDeAdBeEf")))
~~~
# FORMATTED
~~~roc
(
	0X42,
	0x42,
	0B01,
	0b01,
	0O42,
	0o42,
	0.1e42,
	0.1E42,
	0xDEADBEEF,
	0xdeadbeef,
	0xDeAdBeEf,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-13.2
	(elems
		(e-int @2.5-2.9 (value "66"))
		(e-int @3.5-3.9 (value "66"))
		(e-int @4.5-4.9 (value "1"))
		(e-int @5.5-5.9 (value "1"))
		(e-int @6.5-6.9 (value "34"))
		(e-int @7.5-7.9 (value "34"))
		(e-frac-f64 @8.5-8.11 (value "1e41"))
		(e-frac-f64 @9.5-9.11 (value "1e41"))
		(e-int @10.5-10.15 (value "3735928559"))
		(e-int @11.5-11.15 (value "3735928559"))
		(e-int @12.5-12.15 (value "3735928559"))))
~~~
# TYPES
~~~clojure
(expr @1.1-13.2 (type "(Int(*), Int(*), Int(*), Int(*), Int(*), Int(*), Frac(*), Frac(*), Int(*), Int(*), Int(*))"))
~~~
