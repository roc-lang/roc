# META
~~~ini
description=Multiline list with type mismatch
type=expr
~~~
# SOURCE
~~~roc
[
    42,
    "hello world",
    100
]
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - can_list_multiline_mismatch.md:2:5:2:7
TYPE DOES NOT HAVE METHODS - can_list_multiline_mismatch.md:4:5:4:8
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_multiline_mismatch.md:2:5:2:7:**
```roc
    42,
```
    ^^

This type doesn't support methods:
    _Str_



**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_int_digits` on a type that doesn't support methods:
**can_list_multiline_mismatch.md:4:5:4:8:**
```roc
    100
```
    ^^^

This type doesn't support methods:
    _Str_



# TOKENS
~~~zig
OpenSquare,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,
Int,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "hello world")))
	(e-int (raw "100")))
~~~
# FORMATTED
~~~roc
[
	42,
	"hello world",
	100,
]
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-num (value "42"))
		(e-string
			(e-literal (string "hello world")))
		(e-num (value "100"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Str)"))
~~~
