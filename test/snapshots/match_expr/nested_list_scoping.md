# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
}
~~~
# EXPECTED
UNDEFINED VARIABLE - nested_list_scoping.md:1:7:1:17
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `nestedList` in this scope.
Is there an `import` or `exposing` missing up-top?

**nested_list_scoping.md:1:7:1:17:**
```roc
match nestedList {
```
      ^^^^^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:17),OpenCurly(1:18-1:19),
OpenSquare(2:5-2:6),OpenSquare(2:6-2:7),LowerIdent(2:7-2:8),CloseSquare(2:8-2:9),Comma(2:9-2:10),OpenSquare(2:11-2:12),LowerIdent(2:12-2:13),CloseSquare(2:13-2:14),CloseSquare(2:14-2:15),OpFatArrow(2:16-2:18),LowerIdent(2:19-2:20),OpPlus(2:21-2:22),LowerIdent(2:23-2:24),
OpenSquare(3:5-3:6),OpenSquare(3:6-3:7),LowerIdent(3:7-3:8),Comma(3:8-3:9),LowerIdent(3:10-3:11),CloseSquare(3:11-3:12),CloseSquare(3:12-3:13),OpFatArrow(3:14-3:16),LowerIdent(3:17-3:18),OpBinaryMinus(3:19-3:20),LowerIdent(3:21-3:22),
OpenSquare(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),OpenSquare(4:9-4:10),LowerIdent(4:10-4:11),CloseSquare(4:11-4:12),CloseSquare(4:12-4:13),OpFatArrow(4:14-4:16),LowerIdent(4:17-4:18),OpStar(4:19-4:20),LowerIdent(4:21-4:22),
CloseCurly(5:1-5:2),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.17 (raw "nestedList"))
	(branches
		(branch @2.5-2.24
			(p-list @2.5-2.15
				(p-list @2.6-2.9
					(p-ident @2.7-2.8 (raw "x")))
				(p-list @2.11-2.14
					(p-ident @2.12-2.13 (raw "y"))))
			(e-binop @2.19-2.24 (op "+")
				(e-ident @2.19-2.20 (raw "x"))
				(e-ident @2.23-2.24 (raw "y"))))
		(branch @3.5-3.22
			(p-list @3.5-3.13
				(p-list @3.6-3.12
					(p-ident @3.7-3.8 (raw "x"))
					(p-ident @3.10-3.11 (raw "y"))))
			(e-binop @3.17-3.22 (op "-")
				(e-ident @3.17-3.18 (raw "x"))
				(e-ident @3.21-3.22 (raw "y"))))
		(branch @4.5-4.22
			(p-list @4.5-4.13
				(p-ident @4.6-4.7 (raw "x"))
				(p-list @4.9-4.12
					(p-ident @4.10-4.11 (raw "y"))))
			(e-binop @4.17-4.22 (op "*")
				(e-ident @4.17-4.18 (raw "x"))
				(e-ident @4.21-4.22 (raw "y"))))))
~~~
# FORMATTED
~~~roc
match nestedList {
	[[x], [y]] => x + y
	[[x, y]] => x - y
	[x, [y]] => x * y
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @2.5-2.15
							(patterns
								(p-list @2.6-2.9
									(patterns
										(p-assign @2.7-2.8 (ident "x"))))
								(p-list @2.11-2.14
									(patterns
										(p-assign @2.12-2.13 (ident "y"))))))))
				(value
					(e-binop @2.19-2.24 (op "add")
						(e-lookup-local @2.19-2.20
							(p-assign @2.7-2.8 (ident "x")))
						(e-lookup-local @2.23-2.24
							(p-assign @2.12-2.13 (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @3.5-3.13
							(patterns
								(p-list @3.6-3.12
									(patterns
										(p-assign @3.7-3.8 (ident "x"))
										(p-assign @3.10-3.11 (ident "y"))))))))
				(value
					(e-binop @3.17-3.22 (op "sub")
						(e-lookup-local @3.17-3.18
							(p-assign @3.7-3.8 (ident "x")))
						(e-lookup-local @3.21-3.22
							(p-assign @3.10-3.11 (ident "y"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list @4.5-4.13
							(patterns
								(p-assign @4.6-4.7 (ident "x"))
								(p-list @4.9-4.12
									(patterns
										(p-assign @4.10-4.11 (ident "y"))))))))
				(value
					(e-binop @4.17-4.22 (op "mul")
						(e-lookup-local @4.17-4.18
							(p-assign @4.6-4.7 (ident "x")))
						(e-lookup-local @4.21-4.22
							(p-assign @4.10-4.11 (ident "y")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
