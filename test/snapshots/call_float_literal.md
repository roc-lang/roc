# META
~~~ini
description=Calling a float literal as a function should produce an error, not crash
type=expr
~~~
# SOURCE
~~~roc
0.0()
~~~
# EXPECTED
MISSING METHOD - call_float_literal.md:1:1:1:4
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**call_float_literal.md:1:1:1:4:**
```roc
0.0()
```
^^^

The value's type, which does not have a method named **from_numeral**, is:

    ({}) -> _ret

# TOKENS
~~~zig
Float,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-frac (raw "0.0")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-dec-small (numerator "0") (denominator-power-of-ten "1") (value "0.0")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
