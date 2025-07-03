# META
~~~ini
description=Match expression with nested patterns (tags containing records, lists with tags)
type=expr
~~~
# SOURCE
~~~roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: record pattern with sub-patterns
Let us know if you want to help!

**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `len` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `rest` in this scope.
Is there an `import` or `exposing` missing up-top?

**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: record pattern with sub-patterns
Let us know if you want to help!

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:11),OpenCurly(1:12-1:13),Newline(1:1-1:1),
UpperIdent(2:5-2:14),NoSpaceOpenRound(2:14-2:15),OpenCurly(2:15-2:16),LowerIdent(2:17-2:22),OpColon(2:22-2:23),OpenSquare(2:24-2:25),UpperIdent(2:25-2:30),NoSpaceOpenRound(2:30-2:31),LowerIdent(2:31-2:32),CloseRound(2:32-2:33),Comma(2:33-2:34),DoubleDot(2:35-2:37),KwAs(2:38-2:40),LowerIdent(2:41-2:45),CloseSquare(2:45-2:46),CloseCurly(2:47-2:48),CloseRound(2:48-2:49),OpFatArrow(2:50-2:52),LowerIdent(2:53-2:54),OpPlus(2:55-2:56),UpperIdent(2:57-2:61),NoSpaceDotLowerIdent(2:61-2:65),NoSpaceOpenRound(2:65-2:66),LowerIdent(2:66-2:70),CloseRound(2:70-2:71),Newline(1:1-1:1),
UpperIdent(3:5-3:14),NoSpaceOpenRound(3:14-3:15),OpenCurly(3:15-3:16),LowerIdent(3:17-3:22),OpColon(3:22-3:23),OpenSquare(3:24-3:25),CloseSquare(3:25-3:26),CloseCurly(3:27-3:28),CloseRound(3:28-3:29),OpFatArrow(3:30-3:32),Int(3:33-3:34),Newline(1:1-1:1),
UpperIdent(4:5-4:12),NoSpaceOpenRound(4:12-4:13),OpenSquare(4:13-4:14),UpperIdent(4:14-4:17),NoSpaceOpenRound(4:17-4:18),LowerIdent(4:18-4:23),CloseRound(4:23-4:24),Comma(4:24-4:25),UpperIdent(4:26-4:31),NoSpaceOpenRound(4:31-4:32),LowerIdent(4:32-4:33),CloseRound(4:33-4:34),CloseSquare(4:34-4:35),CloseRound(4:35-4:36),OpFatArrow(4:37-4:39),LowerIdent(4:40-4:45),OpPlus(4:46-4:47),LowerIdent(4:48-4:49),Newline(1:1-1:1),
UpperIdent(5:5-5:11),NoSpaceOpenRound(5:11-5:12),LowerIdent(5:12-5:13),CloseRound(5:13-5:14),OpFatArrow(5:15-5:17),LowerIdent(5:18-5:19),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.11 (qaul "") (raw "data"))
	(branches
		(branch @2.5-3.14
			(p-tag @2.5-2.49 (raw "Container")
				(p-record @2.15-2.48
					(field @2.17-2.48 (name "items") (rest false)
						(p-list @2.24-2.46
							(p-tag @2.25-2.33 (raw "First")
								(p-ident @2.31-2.32 (raw "x")))
							(p-list-rest @2.35-2.46 (name "rest"))))))
			(e-binop @2.53-3.14 (op "+")
				(e-ident @2.53-2.54 (qaul "") (raw "x"))
				(e-apply @2.57-2.71
					(e-ident @2.57-2.65 (qaul "List") (raw ".len"))
					(e-ident @2.66-2.70 (qaul "") (raw "rest")))))
		(branch @3.5-4.12
			(p-tag @3.5-3.29 (raw "Container")
				(p-record @3.15-3.28
					(field @3.17-3.28 (name "items") (rest false)
						(p-list @3.24-3.26))))
			(e-int @3.33-3.34 (raw "0")))
		(branch @4.5-5.11
			(p-tag @4.5-4.36 (raw "Wrapper")
				(p-list @4.13-4.35
					(p-tag @4.14-4.24 (raw "Tag")
						(p-ident @4.18-4.23 (raw "value")))
					(p-tag @4.26-4.34 (raw "Other")
						(p-ident @4.32-4.33 (raw "y")))))
			(e-binop @4.40-5.11 (op "+")
				(e-ident @4.40-4.45 (qaul "") (raw "value"))
				(e-ident @4.48-4.49 (qaul "") (raw "y"))))
		(branch @5.5-6.2
			(p-tag @5.5-5.14 (raw "Simple")
				(p-ident @5.12-5.13 (raw "x")))
			(e-ident @5.18-5.19 (qaul "") (raw "x")))))
~~~
# FORMATTED
~~~roc
match data {
	Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
	Container({ items: [] }) => 0
	Wrapper([Tag(value), Other(y)]) => value + y
	Simple(x) => x
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-6.2
	(match @1.1-6.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-applied-tag @2.5-2.49 (degenerate false)))
				(value
					(e-binop @2.53-3.14 (op "add")
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-call @2.57-2.71
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-runtime-error (tag "ident_not_in_scope"))))))
			(branch
				(patterns
					(p-applied-tag @3.5-3.29 (degenerate false)))
				(value
					(e-int @3.33-3.34 (value "0"))))
			(branch
				(patterns
					(p-applied-tag @4.5-4.36 (degenerate false)))
				(value
					(e-binop @4.40-5.11 (op "add")
						(e-lookup-local @4.40-4.45
							(pattern @4.18-4.23))
						(e-lookup-local @4.48-4.49
							(pattern @4.32-4.33)))))
			(branch
				(patterns
					(p-applied-tag @5.5-5.14 (degenerate false)))
				(value
					(e-lookup-local @5.18-5.19
						(pattern @5.12-5.13)))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "a"))
~~~
