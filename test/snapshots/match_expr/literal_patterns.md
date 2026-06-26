# META
~~~ini
description=Match expression with tag patterns for different cases
type=expr
~~~
# SOURCE
~~~roc
match Answer {
    Answer => 1
    Zero => "hello"
    Greeting => 3
    10 => 4
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - literal_patterns.md:1:7:1:13
MISSING METHOD - literal_patterns.md:5:5:5:7
TYPE MISMATCH - literal_patterns.md:3:13:3:20
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This match value is known at compile time, so ───┐
└┬────────────────────────┘  this match will always inspect the same value.   │
 │                                                                            │
 │  match Answer {                                                            │
 │        ‾‾‾‾‾‾                                                              │
 └─────────────────────────────────────────────────── literal_patterns.md:1:7 ┘



┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  10 => 4                                                                   │
 │  ‾‾                                                                        │
 └─────────────────────────────────────────────────── literal_patterns.md:5:5 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        [Answer, Greeting, Zero, ..]


┌───────────────┐
│ TYPE MISMATCH ├─ This string literal is being used where a non-string ──────┐
└┬──────────────┘  type is needed.                                            │
 │                                                                            │
 │  Zero => "hello"                                                           │
 │          ‾‾‾‾‾‾‾                                                           │
 └────────────────────────────────────────────────── literal_patterns.md:3:13 ┘

    The type was determined to be:

        Dec

# TOKENS
~~~zig
KwMatch,UpperIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,Int,
Int,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-tag (raw "Answer"))
	(branches
		(branch
			(p-tag (raw "Answer"))
			(e-int (raw "1")))
		(branch
			(p-tag (raw "Zero"))
			(e-string
				(e-string-part (raw "hello"))))
		(branch
			(p-tag (raw "Greeting"))
			(e-int (raw "3")))
		(branch
			(p-int (raw "10"))
			(e-int (raw "4")))))
~~~
# FORMATTED
~~~roc
match Answer {
	Answer => 1
	Zero => "hello"
	Greeting => 3
	10 => 4
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tag (name "Answer")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-string
						(e-literal (string "hello")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "3"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-num (value "10"))))
				(value
					(e-num (value "4")))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
