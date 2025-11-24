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
INCOMPATIBLE MATCH BRANCHES - literal_patterns.md:1:1:1:1
TYPE DOES NOT HAVE METHODS - literal_patterns.md:5:5:5:7
# PROBLEMS
**INCOMPATIBLE MATCH BRANCHES**
The second branch's type in this `match` is different from the previous ones:
**literal_patterns.md:1:1:**
```roc
match Answer {
    Answer => 1
    Zero => "hello"
```
            ^^^^^^^

The second branch has this type;
    __a where [b.try_from_str : b]_

But the previous branch has this type:
    __a where [b.from_numeral : b]_

All branches in an `match` must have compatible types.

Note: You can wrap branches values in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_numeral` on a type that doesn't support methods:
**literal_patterns.md:5:5:5:7:**
```roc
    10 => 4
```
    ^^

This type doesn't support methods:
    _[Answer, Zero, Greeting]_others_



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
(expr (type "Error"))
~~~
