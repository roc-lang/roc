# META
~~~ini
description=Heterogeneous list where first element is concrete
type=expr
~~~
# SOURCE
~~~roc
[42, "world", 3.14]
~~~
# EXPECTED
MISSING METHOD - can_list_first_concrete.md:1:2:1:4
MISSING METHOD - can_list_first_concrete.md:1:15:1:19
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**can_list_first_concrete.md:1:2:1:4:**
```roc
[42, "world", 3.14]
```
 ^^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**can_list_first_concrete.md:1:15:1:19:**
```roc
[42, "world", 3.14]
```
              ^^^^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,Float,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "world")))
	(e-frac (raw "3.14")))
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
		(e-string
			(e-literal (string "world")))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
