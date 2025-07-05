# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# EXPECTED
UNDEFINED VARIABLE - branch_scoping.md:1:7:1:13
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `result` in this scope.
Is there an `import` or `exposing` missing up-top?

**branch_scoping.md:1:7:1:13:**
```roc
match result {
```
      ^^^^^^


# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:13),OpenCurly(1:14-1:15),Newline(1:1-1:1),
UpperIdent(2:5-2:7),NoSpaceOpenRound(2:7-2:8),LowerIdent(2:8-2:13),CloseRound(2:13-2:14),OpFatArrow(2:15-2:17),LowerIdent(2:18-2:23),OpPlus(2:24-2:25),Int(2:26-2:27),Newline(1:1-1:1),
UpperIdent(3:5-3:8),NoSpaceOpenRound(3:8-3:9),LowerIdent(3:9-3:14),CloseRound(3:14-3:15),OpFatArrow(3:16-3:18),LowerIdent(3:19-3:24),OpBinaryMinus(3:25-3:26),Int(3:27-3:28),Newline(1:1-1:1),
UpperIdent(4:5-4:7),NoSpaceOpenRound(4:7-4:8),LowerIdent(4:8-4:17),CloseRound(4:17-4:18),OpFatArrow(4:19-4:21),LowerIdent(4:22-4:31),OpStar(4:32-4:33),Int(4:34-4:35),Newline(1:1-1:1),
UpperIdent(5:5-5:8),NoSpaceOpenRound(5:8-5:9),LowerIdent(5:9-5:18),CloseRound(5:18-5:19),OpFatArrow(5:20-5:22),LowerIdent(5:23-5:32),OpSlash(5:33-5:34),Int(5:35-5:36),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.13 (raw "result"))
	(branches
		(branch @2.5-3.8
			(p-tag @2.5-2.14 (raw "Ok")
				(p-ident @2.8-2.13 (raw "value")))
			(e-binop @2.18-3.8 (op "+")
				(e-ident @2.18-2.23 (raw "value"))
				(e-int @2.26-2.27 (raw "1"))))
		(branch @3.5-4.7
			(p-tag @3.5-3.15 (raw "Err")
				(p-ident @3.9-3.14 (raw "value")))
			(e-binop @3.19-4.7 (op "-")
				(e-ident @3.19-3.24 (raw "value"))
				(e-int @3.27-3.28 (raw "1"))))
		(branch @4.5-5.8
			(p-tag @4.5-4.18 (raw "Ok")
				(p-ident @4.8-4.17 (raw "different")))
			(e-binop @4.22-5.8 (op "*")
				(e-ident @4.22-4.31 (raw "different"))
				(e-int @4.34-4.35 (raw "2"))))
		(branch @5.5-6.2
			(p-tag @5.5-5.19 (raw "Err")
				(p-ident @5.9-5.18 (raw "different")))
			(e-binop @5.23-6.2 (op "/")
				(e-ident @5.23-5.32 (raw "different"))
				(e-int @5.35-5.36 (raw "2"))))))
~~~
# FORMATTED
~~~roc
match result {
	Ok(value) => value + 1
	Err(value) => value - 1
	Ok(different) => different * 2
	Err(different) => different / 2
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
					(p-applied-tag @2.5-2.14 (degenerate false)))
				(value
					(e-binop @2.18-3.8 (op "add")
						(e-lookup-local @2.18-2.23
							(pattern @2.8-2.13))
						(e-int @2.26-2.27 (value "1")))))
			(branch
				(patterns
					(p-applied-tag @3.5-3.15 (degenerate false)))
				(value
					(e-binop @3.19-4.7 (op "sub")
						(e-lookup-local @3.19-3.24
							(pattern @3.9-3.14))
						(e-int @3.27-3.28 (value "1")))))
			(branch
				(patterns
					(p-applied-tag @4.5-4.18 (degenerate false)))
				(value
					(e-binop @4.22-5.8 (op "mul")
						(e-lookup-local @4.22-4.31
							(pattern @4.8-4.17))
						(e-int @4.34-4.35 (value "2")))))
			(branch
				(patterns
					(p-applied-tag @5.5-5.19 (degenerate false)))
				(value
					(e-binop @5.23-6.2 (op "div")
						(e-lookup-local @5.23-5.32
							(pattern @5.9-5.18))
						(e-int @5.35-5.36 (value "2"))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "*"))
~~~
