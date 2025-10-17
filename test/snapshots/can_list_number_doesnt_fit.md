# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1u8, 2u8, 300]
~~~
# EXPECTED
NUMBER DOES NOT FIT IN TYPE - can_list_number_doesnt_fit.md:1:12:1:15
# PROBLEMS
**NUMBER DOES NOT FIT IN TYPE**
The number **300** does not fit in its inferred type:
**can_list_number_doesnt_fit.md:1:12:1:15:**
```roc
[1u8, 2u8, 300]
```
           ^^^

Its inferred type is:
    _Num(Int(Unsigned8))_

# TOKENS
~~~zig
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1u8"))
	(e-int (raw "2u8"))
	(e-int (raw "300")))
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
		(e-num (value "2"))
		(e-num (value "300"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
