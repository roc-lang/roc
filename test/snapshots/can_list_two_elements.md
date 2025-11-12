# META
~~~ini
description=List with exactly two incompatible elements
type=expr
~~~
# SOURCE
~~~roc
[1, "hello"]
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - can_list_two_elements.md:1:2:1:3
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_two_elements.md:1:2:1:3:**
```roc
[1, "hello"]
```
 ^

This type doesn't support methods:
    _Str_



# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
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
		(e-num (value "1"))
		(e-string
			(e-literal (string "hello")))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
