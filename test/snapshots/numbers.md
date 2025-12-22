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
(expr (type "(a, b, c, d, e, f, g, h, i, j, k)
  where [
    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
    c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]),
    d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]),
    e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]),
    f.from_numeral : Numeral -> Try(f, [InvalidNumeral(Str)]),
    g.from_numeral : Numeral -> Try(g, [InvalidNumeral(Str)]),
    h.from_numeral : Numeral -> Try(h, [InvalidNumeral(Str)]),
    i.from_numeral : Numeral -> Try(i, [InvalidNumeral(Str)]),
    j.from_numeral : Numeral -> Try(j, [InvalidNumeral(Str)]),
    k.from_numeral : Numeral -> Try(k, [InvalidNumeral(Str)]),
  ]"))
~~~
