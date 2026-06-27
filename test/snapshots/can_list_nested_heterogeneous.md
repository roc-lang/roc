# META
~~~ini
description=Heterogeneous nested list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [1], ["hello"]]
~~~
# EXPECTED
TYPE MISMATCH - can_list_nested_heterogeneous.md:1:12:1:19
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  [[], [1], ["hello"]]                                                      │
 │             ‾‾‾‾‾‾‾                                                        │
 └───────────────────────────────────── can_list_nested_heterogeneous.md:1:12 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
OpenSquare,OpenSquare,CloseSquare,Comma,OpenSquare,Int,CloseSquare,Comma,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list)
	(e-list
		(e-int (raw "1")))
	(e-list
		(e-string
			(e-string-part (raw "hello")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-empty_list)
		(e-list
			(elems
				(e-num (value "1"))))
		(e-list
			(elems
				(e-string
					(e-literal (string "hello")))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Dec))"))
~~~
