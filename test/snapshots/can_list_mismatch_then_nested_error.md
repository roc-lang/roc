# META
~~~ini
description=List with type mismatch followed by nested heterogeneous list
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", [3, "world"]]
~~~
# EXPECTED
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:2:1:3
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:5:1:12
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:18:1:25
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This number is being used where a non-number type is ──────┐
└┬──────────────┘  needed.                                                    │
 │                                                                            │
 │  [1, "hello", [3, "world"]]                                                │
 │   ‾                                                                        │
 └──────────────────────────────── can_list_mismatch_then_nested_error.md:1:2 ┘

    The type was determined to be non-numeric here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  [1, "hello", [3, "world"]]                                           │
      │               ‾‾‾‾‾‾‾‾‾‾‾‾                                            │
      └────────────────────────── can_list_mismatch_then_nested_error.md:1:14 ┘
    Other code expects this to have the type:

        List(a)
          where [
            a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
            a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),
          ]


┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  [1, "hello", [3, "world"]]                                                │
 │      ‾‾‾‾‾‾‾                                                               │
 └──────────────────────────────── can_list_mismatch_then_nested_error.md:1:5 ┘

    The type was determined to be:

        List(a)
          where [
            a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
            a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),
          ]


┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  [1, "hello", [3, "world"]]                                                │
 │                   ‾‾‾‾‾‾‾                                                  │
 └─────────────────────────────── can_list_mismatch_then_nested_error.md:1:18 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
	(e-list
		(e-int (raw "3"))
		(e-string
			(e-string-part (raw "world")))))
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
		(e-list
			(elems
				(e-num (value "3"))
				(e-string
					(e-literal (string "world")))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Dec))"))
~~~
