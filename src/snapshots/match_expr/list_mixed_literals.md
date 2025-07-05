# META
~~~ini
description=Match expression with mixed literal and variable patterns in lists
type=expr
~~~
# SOURCE
~~~roc
match sequence {
    [0, count] => count
    [1, x, 3] => x
    [42, value] => value
    [first, 99] => first
    [] => 0
}
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `sequence` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:15),OpenCurly(1:16-1:17),Newline(1:1-1:1),
OpenSquare(2:5-2:6),Int(2:6-2:7),Comma(2:7-2:8),LowerIdent(2:9-2:14),CloseSquare(2:14-2:15),OpFatArrow(2:16-2:18),LowerIdent(2:19-2:24),Newline(1:1-1:1),
OpenSquare(3:5-3:6),Int(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),Comma(3:10-3:11),Int(3:12-3:13),CloseSquare(3:13-3:14),OpFatArrow(3:15-3:17),LowerIdent(3:18-3:19),Newline(1:1-1:1),
OpenSquare(4:5-4:6),Int(4:6-4:8),Comma(4:8-4:9),LowerIdent(4:10-4:15),CloseSquare(4:15-4:16),OpFatArrow(4:17-4:19),LowerIdent(4:20-4:25),Newline(1:1-1:1),
OpenSquare(5:5-5:6),LowerIdent(5:6-5:11),Comma(5:11-5:12),Int(5:13-5:15),CloseSquare(5:15-5:16),OpFatArrow(5:17-5:19),LowerIdent(5:20-5:25),Newline(1:1-1:1),
OpenSquare(6:5-6:6),CloseSquare(6:6-6:7),OpFatArrow(6:8-6:10),Int(6:11-6:12),Newline(1:1-1:1),
CloseCurly(7:1-7:2),Newline(1:1-1:1),
MalformedUnknownToken(8:1-8:2),MalformedUnknownToken(8:2-8:3),MalformedUnknownToken(8:3-8:4),EndOfFile(8:4-8:4),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.15 (raw "sequence"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.15
				(p-int @2.6-2.7 (raw "0"))
				(p-ident @2.9-2.14 (raw "count")))
			(e-ident @2.19-2.24 (raw "count")))
		(branch @3.5-4.6
			(p-list @3.5-3.14
				(p-int @3.6-3.7 (raw "1"))
				(p-ident @3.9-3.10 (raw "x"))
				(p-int @3.12-3.13 (raw "3")))
			(e-ident @3.18-3.19 (raw "x")))
		(branch @4.5-5.6
			(p-list @4.5-4.16
				(p-int @4.6-4.8 (raw "42"))
				(p-ident @4.10-4.15 (raw "value")))
			(e-ident @4.20-4.25 (raw "value")))
		(branch @5.5-6.6
			(p-list @5.5-5.16
				(p-ident @5.6-5.11 (raw "first"))
				(p-int @5.13-5.15 (raw "99")))
			(e-ident @5.20-5.25 (raw "first")))
		(branch @6.5-7.2
			(p-list @6.5-6.7)
			(e-int @6.11-6.12 (raw "0")))))
~~~
# FORMATTED
~~~roc
match sequence {
	[0, count] => count
	[1, x, 3] => x
	[42, value] => value
	[first, 99] => first
	[] => 0
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-7.2
	(match @1.1-7.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-list @2.5-2.15 (degenerate false)
						(patterns
							(p-int @2.6-2.7)
							(p-assign @2.9-2.14 (ident "count")))))
				(value
					(e-lookup-local @2.19-2.24
						(pattern @2.9-2.14))))
			(branch
				(patterns
					(p-list @3.5-3.14 (degenerate false)
						(patterns
							(p-int @3.6-3.7)
							(p-assign @3.9-3.10 (ident "x"))
							(p-int @3.12-3.13))))
				(value
					(e-lookup-local @3.18-3.19
						(pattern @3.9-3.10))))
			(branch
				(patterns
					(p-list @4.5-4.16 (degenerate false)
						(patterns
							(p-int @4.6-4.8)
							(p-assign @4.10-4.15 (ident "value")))))
				(value
					(e-lookup-local @4.20-4.25
						(pattern @4.10-4.15))))
			(branch
				(patterns
					(p-list @5.5-5.16 (degenerate false)
						(patterns
							(p-assign @5.6-5.11 (ident "first"))
							(p-int @5.13-5.15))))
				(value
					(e-lookup-local @5.20-5.25
						(pattern @5.6-5.11))))
			(branch
				(patterns
					(p-list @6.5-6.7 (degenerate false)
						(patterns)))
				(value
					(e-int @6.11-6.12 (value "0")))))))
~~~
# TYPES
~~~clojure
(expr @1.1-7.2 (type "Num(*)"))
~~~
