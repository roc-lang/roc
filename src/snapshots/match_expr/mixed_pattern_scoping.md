# META
~~~ini
description=Match expression with mixed tag and list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - mixed_pattern_scoping.md:1:7:1:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**mixed_pattern_scoping.md:1:7:1:11:**
```roc
match data {
```
      ^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:11),OpenCurly(1:12-1:13),Newline(1:1-1:1),
UpperIdent(2:5-2:7),NoSpaceOpenRound(2:7-2:8),OpenSquare(2:8-2:9),LowerIdent(2:9-2:10),Comma(2:10-2:11),LowerIdent(2:12-2:13),CloseSquare(2:13-2:14),CloseRound(2:14-2:15),OpFatArrow(2:16-2:18),LowerIdent(2:19-2:20),OpPlus(2:21-2:22),LowerIdent(2:23-2:24),Newline(1:1-1:1),
UpperIdent(3:5-3:8),NoSpaceOpenRound(3:8-3:9),LowerIdent(3:9-3:10),CloseRound(3:10-3:11),OpFatArrow(3:12-3:14),LowerIdent(3:15-3:16),OpBinaryMinus(3:17-3:18),Int(3:19-3:20),Newline(1:1-1:1),
UpperIdent(4:5-4:7),NoSpaceOpenRound(4:7-4:8),OpenSquare(4:8-4:9),LowerIdent(4:9-4:10),CloseSquare(4:10-4:11),CloseRound(4:11-4:12),OpFatArrow(4:13-4:15),LowerIdent(4:16-4:17),OpStar(4:18-4:19),Int(4:20-4:21),Newline(1:1-1:1),
UpperIdent(5:5-5:8),NoSpaceOpenRound(5:8-5:9),LowerIdent(5:9-5:10),CloseRound(5:10-5:11),OpFatArrow(5:12-5:14),LowerIdent(5:15-5:16),OpSlash(5:17-5:18),Int(5:19-5:20),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.11 (raw "data"))
	(branches
		(branch @2.5-3.8
			(p-tag @2.5-2.15 (raw "Ok")
				(p-list @2.8-2.14
					(p-ident @2.9-2.10 (raw "x"))
					(p-ident @2.12-2.13 (raw "y"))))
			(e-binop @2.19-3.8 (op "+")
				(e-ident @2.19-2.20 (raw "x"))
				(e-ident @2.23-2.24 (raw "y"))))
		(branch @3.5-4.7
			(p-tag @3.5-3.11 (raw "Err")
				(p-ident @3.9-3.10 (raw "x")))
			(e-binop @3.15-4.7 (op "-")
				(e-ident @3.15-3.16 (raw "x"))
				(e-int @3.19-3.20 (raw "1"))))
		(branch @4.5-5.8
			(p-tag @4.5-4.12 (raw "Ok")
				(p-list @4.8-4.11
					(p-ident @4.9-4.10 (raw "x"))))
			(e-binop @4.16-5.8 (op "*")
				(e-ident @4.16-4.17 (raw "x"))
				(e-int @4.20-4.21 (raw "2"))))
		(branch @5.5-6.2
			(p-tag @5.5-5.11 (raw "Err")
				(p-ident @5.9-5.10 (raw "y")))
			(e-binop @5.15-6.2 (op "/")
				(e-ident @5.15-5.16 (raw "y"))
				(e-int @5.19-5.20 (raw "2"))))))
~~~
# FORMATTED
~~~roc
match data {
	Ok([x, y]) => x + y
	Err(x) => x - 1
	Ok([x]) => x * 2
	Err(y) => y / 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-6.2
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch @2.19-3.8
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @2.5-2.15)))
				(value
					(e-binop @2.19-3.8 (op "add")
						(e-lookup-local @2.19-2.20
							(p-assign @2.9-2.10 (ident "x")))
						(e-lookup-local @2.23-2.24
							(p-assign @2.12-2.13 (ident "y"))))))
			(branch @3.15-4.7
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @3.5-3.11)))
				(value
					(e-binop @3.15-4.7 (op "sub")
						(e-lookup-local @3.15-3.16
							(p-assign @3.9-3.10 (ident "x")))
						(e-int @3.19-3.20 (value "1")))))
			(branch @4.16-5.8
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @4.5-4.12)))
				(value
					(e-binop @4.16-5.8 (op "mul")
						(e-lookup-local @4.16-4.17
							(p-assign @4.9-4.10 (ident "x")))
						(e-int @4.20-4.21 (value "2")))))
			(branch @5.15-6.2
				(patterns
					(pattern (degenerate false)
						(p-applied-tag @5.5-5.11)))
				(value
					(e-binop @5.15-6.2 (op "div")
						(e-lookup-local @5.15-5.16
							(p-assign @5.9-5.10 (ident "y")))
						(e-int @5.19-5.20 (value "2"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "*"))
~~~
