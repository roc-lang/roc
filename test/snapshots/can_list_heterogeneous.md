# META
~~~ini
description=Heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", 3.14]
~~~
# EXPECTED
MISSING METHOD - can_list_heterogeneous.md:1:2:1:3
MISSING METHOD - can_list_heterogeneous.md:1:14:1:18
# PROBLEMS
**MISSING METHOD**
This **from_num_literal** method is being called on the type **Str**, which has no method with that name:
**can_list_heterogeneous.md:1:2:1:3:**
```roc
[1, "hello", 3.14]
```
 ^


**Hint: **For this to work, the type would need to have a method named **from_num_literal** associated with it in the type's declaration.

**MISSING METHOD**
This **from_num_literal** method is being called on the type **Str**, which has no method with that name:
**can_list_heterogeneous.md:1:14:1:18:**
```roc
[1, "hello", 3.14]
```
             ^^^^


**Hint: **For this to work, the type would need to have a method named **from_num_literal** associated with it in the type's declaration.

# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,Float,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
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
		(e-num (value "1"))
		(e-string
			(e-literal (string "hello")))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
