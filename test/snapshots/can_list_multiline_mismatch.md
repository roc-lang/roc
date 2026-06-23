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
TYPE MISMATCH - can_list_multiline_mismatch.md:3:5:3:18
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  "hello world",                                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                             │
 └──────────────────────────────────────── can_list_multiline_mismatch.md:3:5 ┘

    The type was determined to be:

        Dec

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
(expr (type "List(Dec)"))
~~~
