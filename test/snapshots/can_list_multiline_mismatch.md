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
TYPE MISMATCH - can_list_multiline_mismatch.md:2:5:2:7
TYPE MISMATCH - can_list_multiline_mismatch.md:4:5:4:8
# PROBLEMS
**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_multiline_mismatch.md:2:5:2:7:**
```roc
    42,
```
    ^^

The type was determined to be non-numeric here:
**can_list_multiline_mismatch.md:3:5:3:18:**
```roc
    "hello world",
```
    ^^^^^^^^^^^^^

Other code expects this to have the type:

    Str

**TYPE MISMATCH**
This number is being used where a non-number type is needed:
**can_list_multiline_mismatch.md:4:5:4:8:**
```roc
    100
```
    ^^^

Other code expects this to have the type:

    Str

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
