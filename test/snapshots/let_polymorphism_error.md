# META
~~~ini
description=Let-polymorphism error case - incompatible list elements
type=expr
~~~
# SOURCE
~~~roc
[42, 4.2, "hello"]
~~~
# EXPECTED
MISSING METHOD - let_polymorphism_error.md:1:6:1:9
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on a value whose type doesn't have that method:
**let_polymorphism_error.md:1:6:1:9:**
```roc
[42, 4.2, "hello"]
```
     ^^^

The value's type, which does not have a method named **from_numeral**, is:

    _Str_

**Hint:** For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

# TOKENS
~~~zig
OpenSquare,Int,Comma,Float,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-frac (raw "4.2"))
	(e-string
		(e-string-part (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-num (value "42"))
		(e-dec-small (numerator "42") (denominator-power-of-ten "1") (value "4.2"))
		(e-string
			(e-literal (string "hello")))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
