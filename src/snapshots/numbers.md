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
    0xDEADBEEF,
    0xdeadbeef,
    0xDeAdBeEf,
)
~~~
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
Int(9:5-9:15),Comma(9:15-9:16),Newline(1:1-1:1),
Int(10:5-10:15),Comma(10:15-10:16),Newline(1:1-1:1),
Int(11:5-11:15),Comma(11:15-11:16),Newline(1:1-1:1),
CloseRound(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-12.2
	(e-int @2.5-2.9 (raw "0X42"))
	(e-int @3.5-3.9 (raw "0x42"))
	(e-int @4.5-4.9 (raw "0B01"))
	(e-int @5.5-5.9 (raw "0b01"))
	(e-int @6.5-6.9 (raw "0O42"))
	(e-int @7.5-7.9 (raw "0o42"))
	(e-frac @8.5-8.11 (raw "0.1e42"))
	(e-int @9.5-9.15 (raw "0xDEADBEEF"))
	(e-int @10.5-10.15 (raw "0xdeadbeef"))
	(e-int @11.5-11.15 (raw "0xDeAdBeEf")))
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
	0xDEADBEEF,
	0xdeadbeef,
	0xDeAdBeEf,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-12.2 (id 83)
	(elems
		(e-int @2.5-2.9 (value "66"))
		(e-int @3.5-3.9 (value "66"))
		(e-int @4.5-4.9 (value "1"))
		(e-int @5.5-5.9 (value "1"))
		(e-int @6.5-6.9 (value "34"))
		(e-int @7.5-7.9 (value "34"))
		(e-frac-f64 @8.5-8.11 (value "1e41"))
		(e-int @9.5-9.15 (value "3735928559"))
		(e-int @10.5-10.15 (value "3735928559"))
		(e-int @11.5-11.15 (value "3735928559"))))
~~~
# TYPES
~~~clojure
(expr (id 83) (type "(Int(*), Int(*), Int(*), Int(*), Int(*), Int(*), Frac(*), Int(*), Int(*), Int(*))"))
~~~
