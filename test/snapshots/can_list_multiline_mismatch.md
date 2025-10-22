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
NIL
# PROBLEMS
NIL
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
(expr (type "List(Error)"))
~~~
