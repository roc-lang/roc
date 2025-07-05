# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:11),OpenCurly(1:12-1:13),Newline(1:1-1:1),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),CloseSquare(2:11-2:12),OpFatArrow(2:13-2:15),LowerIdent(2:16-2:21),Newline(1:1-1:1),
OpenSquare(3:5-3:6),LowerIdent(3:6-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:19),CloseSquare(3:19-3:20),OpFatArrow(3:21-3:23),LowerIdent(3:24-3:29),OpPlus(3:30-3:31),LowerIdent(3:32-3:38),Newline(1:1-1:1),
CloseCurly(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.11 (raw "list"))
	(branches
		(branch @2.5-3.6
			(p-list @2.5-2.12
				(p-ident @2.6-2.11 (raw "first")))
			(e-ident @2.16-2.21 (raw "first")))
		(branch @3.5-4.2
			(p-list @3.5-3.20
				(p-ident @3.6-3.11 (raw "first"))
				(p-ident @3.13-3.19 (raw "second")))
			(e-binop @3.24-4.2 (op "+")
				(e-ident @3.24-3.29 (raw "first"))
				(e-ident @3.32-3.38 (raw "second"))))))
~~~
# FORMATTED
~~~roc
match list {
	[first] => first
	[first, second] => first + second
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-list @2.5-2.12 (degenerate false)
						(patterns
							(p-assign @2.6-2.11 (ident "first")))))
				(value
					(e-lookup-local @2.16-2.21
						(pattern @2.6-2.11))))
			(branch
				(patterns
					(p-list @3.5-3.20 (degenerate false)
						(patterns
							(p-assign @3.6-3.11 (ident "first"))
							(p-assign @3.13-3.19 (ident "second")))))
				(value
					(e-binop @3.24-4.2 (op "add")
						(e-lookup-local @3.24-3.29
							(pattern @3.6-3.11))
						(e-lookup-local @3.32-3.38
							(pattern @3.13-3.19))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "*"))
~~~
