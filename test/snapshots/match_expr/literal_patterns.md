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
INCOMPATIBLE MATCH PATTERNS - literal_patterns.md:1:1:1:1
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
    _Str_

But the previous branch has this type:
    _Num(_size)_

All branches in an `match` must have compatible types.

Note: You can wrap branches values in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

**INCOMPATIBLE MATCH PATTERNS**
The pattern in the fourth branch of this `match` differs from previous ones:
**literal_patterns.md:1:1:**
```roc
match Answer {
    Answer => 1
    Zero => "hello"
    Greeting => 3
    10 => 4
}
```
    ^^

The fourth pattern has this type:
    _Num(_size)_

But all the previous patterns have this type: 
    _[Answer, Zero, Greeting]_others_

All patterns in an `match` must have compatible types.



# TOKENS
~~~zig
KwMatch(1:1-1:6),UpperIdent(1:7-1:13),OpenCurly(1:14-1:15),
UpperIdent(2:5-2:11),OpFatArrow(2:12-2:14),Int(2:15-2:16),
UpperIdent(3:5-3:9),OpFatArrow(3:10-3:12),StringStart(3:13-3:14),StringPart(3:14-3:19),StringEnd(3:19-3:20),
UpperIdent(4:5-4:13),OpFatArrow(4:14-4:16),Int(4:17-4:18),
Int(5:5-5:7),OpFatArrow(5:8-5:10),Int(5:11-5:12),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-tag @1.7-1.13 (raw "Answer"))
	(branches
		(branch @2.5-2.16
			(p-tag @2.5-2.11 (raw "Answer"))
			(e-int @2.15-2.16 (raw "1")))
		(branch @3.5-3.20
			(p-tag @3.5-3.9 (raw "Zero"))
			(e-string @3.13-3.20
				(e-string-part @3.14-3.19 (raw "hello"))))
		(branch @4.5-4.18
			(p-tag @4.5-4.13 (raw "Greeting"))
			(e-int @4.17-4.18 (raw "3")))
		(branch @5.5-5.12
			(p-int @5.5-5.7 (raw "10"))
			(e-int @5.11-5.12 (raw "4")))))
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
(e-match @1.1-6.2
	(match @1.1-6.2
		(cond
			(e-tag @1.7-1.13 (name "Answer")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @2.5-2.11)))
				(value
					(e-int @2.15-2.16 (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @3.5-3.9)))
				(value
					(e-string @3.13-3.20
						(e-literal @3.14-3.19 (string "hello")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @4.5-4.13)))
				(value
					(e-int @4.17-4.18 (value "3"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-int @5.5-5.7 (value "10"))))
				(value
					(e-int @5.11-5.12 (value "4")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Error"))
~~~
