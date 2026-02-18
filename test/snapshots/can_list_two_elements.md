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
TYPE MISMATCH - can_list_two_elements.md:1:2:1:3
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_two_elements.md:1:2:1:3:**
```roc
[1, "hello"]
```
 ^

The type was determined to be non-numeric here:
**can_list_two_elements.md:1:5:1:12:**
```roc
[1, "hello"]
```
    ^^^^^^^

Other code expects this to have the type:

    Str

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
