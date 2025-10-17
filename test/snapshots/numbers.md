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
UPPERCASE BASE - :0:0:0:0
UPPERCASE BASE - :0:0:0:0
UPPERCASE BASE - :0:0:0:0
# PROBLEMS
**UPPERCASE BASE**
Number base prefixes must be lowercase (0x, 0o, 0b).



**UPPERCASE BASE**
Number base prefixes must be lowercase (0x, 0o, 0b).



**UPPERCASE BASE**
Number base prefixes must be lowercase (0x, 0o, 0b).



# TOKENS
~~~zig
OpenRound,
Int,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
Float,Comma,
Float,Comma,
Int,Comma,
Int,Comma,
Int,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-int (raw "0X42"))
	(e-int (raw "0x42"))
	(e-int (raw "0B01"))
	(e-int (raw "0b01"))
	(e-int (raw "0O42"))
	(e-int (raw "0o42"))
	(e-frac (raw "0.1e42"))
	(e-frac (raw "0.1E42"))
	(e-int (raw "0xDEADBEEF"))
	(e-int (raw "0xdeadbeef"))
	(e-int (raw "0xDeAdBeEf")))
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
(e-tuple
	(elems
		(e-num (value "66"))
		(e-num (value "66"))
		(e-num (value "1"))
		(e-num (value "1"))
		(e-num (value "34"))
		(e-num (value "34"))
		(e-frac-f64 (value "1e41"))
		(e-frac-f64 (value "1e41"))
		(e-num (value "3735928559"))
		(e-num (value "3735928559"))
		(e-num (value "3735928559"))))
~~~
# TYPES
~~~clojure
(expr (type "(Num(Int(_size)), Num(Int(_size2)), Num(Int(_size3)), Num(Int(_size4)), Num(Int(_size5)), Num(Int(_size6)), Num(Frac(_size7)), Num(Frac(_size8)), Num(Int(_size9)), Num(Int(_size10)), Num(Int(_size11)))"))
~~~
