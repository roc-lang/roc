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
MISSING METHOD - can_list_multiline_mismatch.md:2:5:2:7
MISSING METHOD - can_list_multiline_mismatch.md:4:5:4:8
# PROBLEMS
**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**can_list_multiline_mismatch.md:2:5:2:7:**
```roc
    42,
```
    ^^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

**MISSING METHOD**
This **from_numeral** method is being called on the type **Str**, which has no method with that name:
**can_list_multiline_mismatch.md:4:5:4:8:**
```roc
    100
```
    ^^^


**Hint: **For this to work, the type would need to have a method named **from_numeral** associated with it in the type's declaration.

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
