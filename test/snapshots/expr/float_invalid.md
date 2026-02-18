# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# EXPECTED
MISSING METHOD - float_invalid.md:1:1:1:5
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**float_invalid.md:1:1:1:5:**
```roc
3.14.15
```
^^^^

The value's type, which does not have a method named**from_numeral**, is:

    (_field, _field2, _field3, _field4, _field5, _field6, _field7, _field8, _field9, _field10, _field11, _field12, _field13, _field14, _field15, _field16)

# TOKENS
~~~zig
Float,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-frac (raw "3.14"))
	".15")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "15")
	(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
