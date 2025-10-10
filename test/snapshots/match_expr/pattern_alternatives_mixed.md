# META
~~~ini
description=Pattern alternatives with mixed pattern types
type=expr
~~~
# SOURCE
~~~roc
match ... {
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
	Ok(_) | Some(_) => "success value"
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
	_ => "other"
}
~~~
# EXPECTED
INCOMPATIBLE MATCH PATTERNS - pattern_alternatives_mixed.md:1:1:1:1
# PROBLEMS
**INCOMPATIBLE MATCH PATTERNS**
The pattern first pattern in this second`match` differs from previous ones:
**pattern_alternatives_mixed.md:1:1:**
```roc
match ... {
	1 | 2 | 3 => "small numbers"
	"hello" | "world" => "greetings"
	Ok(_) | Some(_) => "success value"
	[] | [_] => "short list"
	(0, _) | (_, 0) => "has zero"
	_ => "other"
}
```
 ^^^^^^^

The second pattern has this type:
    _Str_

But all the previous patterns have this type: 
    _Num(_size)_

All patterns in an `match` must have compatible types.



# TOKENS
~~~zig
KwMatch(1:1-1:6),TripleDot(1:7-1:10),OpenCurly(1:11-1:12),
Int(2:2-2:3),OpBar(2:4-2:5),Int(2:6-2:7),OpBar(2:8-2:9),Int(2:10-2:11),OpFatArrow(2:12-2:14),StringStart(2:15-2:16),StringPart(2:16-2:29),StringEnd(2:29-2:30),
StringStart(3:2-3:3),StringPart(3:3-3:8),StringEnd(3:8-3:9),OpBar(3:10-3:11),StringStart(3:12-3:13),StringPart(3:13-3:18),StringEnd(3:18-3:19),OpFatArrow(3:20-3:22),StringStart(3:23-3:24),StringPart(3:24-3:33),StringEnd(3:33-3:34),
UpperIdent(4:2-4:4),NoSpaceOpenRound(4:4-4:5),Underscore(4:5-4:6),CloseRound(4:6-4:7),OpBar(4:8-4:9),UpperIdent(4:10-4:14),NoSpaceOpenRound(4:14-4:15),Underscore(4:15-4:16),CloseRound(4:16-4:17),OpFatArrow(4:18-4:20),StringStart(4:21-4:22),StringPart(4:22-4:35),StringEnd(4:35-4:36),
OpenSquare(5:2-5:3),CloseSquare(5:3-5:4),OpBar(5:5-5:6),OpenSquare(5:7-5:8),Underscore(5:8-5:9),CloseSquare(5:9-5:10),OpFatArrow(5:11-5:13),StringStart(5:14-5:15),StringPart(5:15-5:25),StringEnd(5:25-5:26),
OpenRound(6:2-6:3),Int(6:3-6:4),Comma(6:4-6:5),Underscore(6:6-6:7),CloseRound(6:7-6:8),OpBar(6:9-6:10),OpenRound(6:11-6:12),Underscore(6:12-6:13),Comma(6:13-6:14),Int(6:15-6:16),CloseRound(6:16-6:17),OpFatArrow(6:18-6:20),StringStart(6:21-6:22),StringPart(6:22-6:30),StringEnd(6:30-6:31),
Underscore(7:2-7:3),OpFatArrow(7:4-7:6),StringStart(7:7-7:8),StringPart(7:8-7:13),StringEnd(7:13-7:14),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch @2.2-2.30
			(p-alternatives
				(p-int @2.2-2.3 (raw "1"))
				(p-int @2.6-2.7 (raw "2"))
				(p-int @2.10-2.11 (raw "3")))
			(e-string @2.15-2.30
				(e-string-part @2.16-2.29 (raw "small numbers"))))
		(branch @3.2-3.34
			(p-alternatives
				(p-string @3.2-3.9 (raw """))
				(p-string @3.12-3.19 (raw """)))
			(e-string @3.23-3.34
				(e-string-part @3.24-3.33 (raw "greetings"))))
		(branch @4.2-4.36
			(p-alternatives
				(p-tag @4.2-4.7 (raw "Ok")
					(p-underscore))
				(p-tag @4.10-4.17 (raw "Some")
					(p-underscore)))
			(e-string @4.21-4.36
				(e-string-part @4.22-4.35 (raw "success value"))))
		(branch @5.2-5.26
			(p-alternatives
				(p-list @5.2-5.4)
				(p-list @5.7-5.10
					(p-underscore)))
			(e-string @5.14-5.26
				(e-string-part @5.15-5.25 (raw "short list"))))
		(branch @6.2-6.31
			(p-alternatives
				(p-tuple @6.2-6.8
					(p-int @6.3-6.4 (raw "0"))
					(p-underscore))
				(p-tuple @6.11-6.17
					(p-underscore)
					(p-int @6.15-6.16 (raw "0"))))
			(e-string @6.21-6.31
				(e-string-part @6.22-6.30 (raw "has zero"))))
		(branch @7.2-7.14
			(p-underscore)
			(e-string @7.7-7.14
				(e-string-part @7.8-7.13 (raw "other"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-8.2
	(match @1.1-8.2
		(cond
			(e-not-implemented @1.1-1.1))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-num @2.2-2.3 (value "1")))
					(pattern (degenerate false)
						(p-num @2.6-2.7 (value "2")))
					(pattern (degenerate false)
						(p-num @2.10-2.11 (value "3"))))
				(value
					(e-string @2.15-2.30
						(e-literal @2.16-2.29 (string "small numbers")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-str @3.2-3.9 (text """)))
					(pattern (degenerate false)
						(p-str @3.12-3.19 (text """))))
				(value
					(e-string @3.23-3.34
						(e-literal @3.24-3.33 (string "greetings")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @4.2-4.7))
					(pattern (degenerate false)
						(p-applied-tag @4.10-4.17)))
				(value
					(e-string @4.21-4.36
						(e-literal @4.22-4.35 (string "success value")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @5.2-5.4
							(patterns)))
					(pattern (degenerate false)
						(p-list @5.7-5.10
							(patterns
								(p-underscore @5.8-5.9)))))
				(value
					(e-string @5.14-5.26
						(e-literal @5.15-5.25 (string "short list")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple @6.2-6.8
							(patterns
								(p-num @6.3-6.4 (value "0"))
								(p-underscore @6.6-6.7))))
					(pattern (degenerate false)
						(p-tuple @6.11-6.17
							(patterns
								(p-underscore @6.12-6.13)
								(p-num @6.15-6.16 (value "0"))))))
				(value
					(e-string @6.21-6.31
						(e-literal @6.22-6.30 (string "has zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore @7.2-7.3)))
				(value
					(e-string @7.7-7.14
						(e-literal @7.8-7.13 (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.2 (type "Str"))
~~~
