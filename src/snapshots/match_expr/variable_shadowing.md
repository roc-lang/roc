# META
~~~ini
description=Match expression demonstrating variable shadowing between outer scope and branches
type=expr
~~~
# SOURCE
~~~roc
match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `value` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `other` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),OpenRound(1:7-1:8),LowerIdent(1:8-1:13),Comma(1:13-1:14),LowerIdent(1:15-1:20),CloseRound(1:20-1:21),OpenCurly(1:22-1:23),Newline(1:1-1:1),
OpenRound(2:5-2:6),UpperIdent(2:6-2:10),NoSpaceOpenRound(2:10-2:11),LowerIdent(2:11-2:12),CloseRound(2:12-2:13),Comma(2:13-2:14),LowerIdent(2:15-2:16),CloseRound(2:16-2:17),OpFatArrow(2:18-2:20),LowerIdent(2:21-2:22),OpPlus(2:23-2:24),LowerIdent(2:25-2:26),Newline(1:1-1:1),
OpenRound(3:5-3:6),UpperIdent(3:6-3:10),Comma(3:10-3:11),LowerIdent(3:12-3:13),CloseRound(3:13-3:14),OpFatArrow(3:15-3:17),LowerIdent(3:18-3:19),OpStar(3:20-3:21),Int(3:22-3:23),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-tuple @1.7-1.21
		(e-ident @1.8-1.13 (raw "value"))
		(e-ident @1.15-1.20 (raw "other")))
	(branches
		(branch @2.5-3.6
			(p-tuple @2.5-2.17
				(p-tag @2.6-2.13 (raw "Some")
					(p-ident @2.11-2.12 (raw "x")))
				(p-ident @2.15-2.16 (raw "y")))
			(e-binop @2.21-3.6 (op "+")
				(e-ident @2.21-2.22 (raw "x"))
				(e-ident @2.25-2.26 (raw "y"))))
		(branch @3.5-4.2
			(p-tuple @3.5-3.14
				(p-tag @3.6-3.10 (raw "None"))
				(p-ident @3.12-3.13 (raw "x")))
			(e-binop @3.18-4.2 (op "*")
				(e-ident @3.18-3.19 (raw "x"))
				(e-int @3.22-3.23 (raw "2"))))))
~~~
# FORMATTED
~~~roc
match (value, other) {
	(Some(x), y) => x + y
	(None, x) => x * 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-tuple @1.7-1.21
				(elems
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-runtime-error (tag "ident_not_in_scope")))))
		(branches
			(branch
				(patterns
					(p-tuple @2.5-2.17 (degenerate false)
						(patterns
							(p-applied-tag @2.6-2.13)
							(p-assign @2.15-2.16 (ident "y")))))
				(value
					(e-binop @2.21-3.6 (op "add")
						(e-lookup-local @2.21-2.22
							(pattern @2.11-2.12))
						(e-lookup-local @2.25-2.26
							(pattern @2.15-2.16)))))
			(branch
				(patterns
					(p-tuple @3.5-3.14 (degenerate false)
						(patterns
							(p-applied-tag @3.6-3.10)
							(p-assign @3.12-3.13 (ident "x")))))
				(value
					(e-binop @3.18-4.2 (op "mul")
						(e-lookup-local @3.18-3.19
							(pattern @3.12-3.13))
						(e-int @3.22-3.23 (value "2"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "*"))
~~~
