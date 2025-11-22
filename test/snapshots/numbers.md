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
(expr (type "(_field, _field2, _field3, _field4, _field5, _field6, _field7, _field8, _field9, _field10, _field11) where [_a.from_num_literal : _arg -> _ret, _b.from_num_literal : _arg2 -> _ret2, _c.from_num_literal : _arg3 -> _ret3, _d.from_num_literal : _arg4 -> _ret4, _e.from_num_literal : _arg5 -> _ret5, _f.from_num_literal : _arg6 -> _ret6, _g.from_num_literal : _arg7 -> _ret7, _h.from_num_literal : _arg8 -> _ret8, _i.from_num_literal : _arg9 -> _ret9, _j.from_num_literal : _arg10 -> _ret10, _k.from_num_literal : _arg11 -> _ret11]"))
~~~
