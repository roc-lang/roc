# META
~~~ini
description=Single quote literal's in patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
	['#'] => ...
	['a', 'b'] => ...
	_ => ...
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch(1:1-1:6),TripleDot(1:7-1:10),OpenCurly(1:11-1:12),Newline(1:1-1:1),
OpenSquare(2:2-2:3),SingleQuote(2:3-2:6),CloseSquare(2:6-2:7),OpFatArrow(2:8-2:10),TripleDot(2:11-2:14),Newline(1:1-1:1),
OpenSquare(3:2-3:3),SingleQuote(3:3-3:6),Comma(3:6-3:7),SingleQuote(3:8-3:11),CloseSquare(3:11-3:12),OpFatArrow(3:13-3:15),TripleDot(3:16-3:19),Newline(1:1-1:1),
Underscore(4:2-4:3),OpFatArrow(4:4-4:6),TripleDot(4:7-4:10),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch @2.2-3.3
			(p-list @2.2-2.7
				(p-single-quote @2.3-2.6 (raw "'#'")))
			(e-ellipsis))
		(branch @3.2-4.3
			(p-list @3.2-3.12
				(p-single-quote @3.3-3.6 (raw "'a'"))
				(p-single-quote @3.8-3.11 (raw "'b'")))
			(e-ellipsis))
		(branch @4.2-5.2
			(p-underscore)
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-not-implemented @1.7-1.10))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.2-2.7
							(patterns
								(p-int @2.3-2.6 (value "35"))))))
				(value
					(e-not-implemented @2.11-2.14)))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.2-3.12
							(patterns
								(p-int @3.3-3.6 (value "97"))
								(p-int @3.8-3.11 (value "98"))))))
				(value
					(e-not-implemented @3.16-3.19)))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore @4.2-4.3)))
				(value
					(e-not-implemented @4.7-4.10))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
