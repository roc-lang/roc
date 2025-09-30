# META
~~~ini
description=Simple addition function with expect statement
type=file
~~~
# SOURCE
~~~roc
addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
~~~
# EXPECTED
MISSING MAIN! FUNCTION - simple_add.md:1:1:5:26
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**simple_add.md:1:1:5:26:**
```roc
addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
```


# TOKENS
~~~zig
LowerIdent(1:1-1:6),OpColon(1:7-1:8),UpperIdent(1:9-1:11),Comma(1:11-1:12),UpperIdent(1:13-1:15),OpArrow(1:16-1:18),UpperIdent(1:19-1:21),
LowerIdent(2:1-2:6),OpAssign(2:7-2:8),OpBar(2:9-2:10),LowerIdent(2:10-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:14),OpBar(2:14-2:15),LowerIdent(2:16-2:17),OpPlus(2:18-2:19),LowerIdent(2:20-2:21),
KwExpect(4:1-4:7),LowerIdent(4:8-4:13),NoSpaceOpenRound(4:13-4:14),Int(4:14-4:15),Comma(4:15-4:16),Int(4:17-4:18),CloseRound(4:18-4:19),OpEquals(4:20-4:22),Int(4:23-4:24),
KwExpect(5:1-5:7),LowerIdent(5:8-5:13),NoSpaceOpenRound(5:13-5:14),Int(5:14-5:15),Comma(5:15-5:16),Int(5:17-5:19),CloseRound(5:19-5:20),OpEquals(5:21-5:23),Int(5:24-5:26),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.26
	(type-module @1.1-1.6)
	(statements
		(s-type-anno @1.1-1.21 (name "addU8")
			(ty-fn @1.9-1.21
				(ty @1.9-1.11 (name "U8"))
				(ty @1.13-1.15 (name "U8"))
				(ty @1.19-1.21 (name "U8"))))
		(s-decl @2.1-2.21
			(p-ident @2.1-2.6 (raw "addU8"))
			(e-lambda @2.9-2.21
				(args
					(p-ident @2.10-2.11 (raw "a"))
					(p-ident @2.13-2.14 (raw "b")))
				(e-binop @2.16-2.21 (op "+")
					(e-ident @2.16-2.17 (raw "a"))
					(e-ident @2.20-2.21 (raw "b")))))
		(s-expect @4.1-4.24
			(e-binop @4.8-4.24 (op "==")
				(e-apply @4.8-4.19
					(e-ident @4.8-4.13 (raw "addU8"))
					(e-int @4.14-4.15 (raw "1"))
					(e-int @4.17-4.18 (raw "2")))
				(e-int @4.23-4.24 (raw "3"))))
		(s-expect @5.1-5.26
			(e-binop @5.8-5.26 (op "==")
				(e-apply @5.8-5.20
					(e-ident @5.8-5.13 (raw "addU8"))
					(e-int @5.14-5.15 (raw "0"))
					(e-int @5.17-5.19 (raw "10")))
				(e-int @5.24-5.26 (raw "10"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.6 (ident "addU8"))
		(e-lambda @2.9-2.21
			(args
				(p-assign @2.10-2.11 (ident "a"))
				(p-assign @2.13-2.14 (ident "b")))
			(e-binop @2.16-2.21 (op "add")
				(e-lookup-local @2.16-2.17
					(p-assign @2.10-2.11 (ident "a")))
				(e-lookup-local @2.20-2.21
					(p-assign @2.13-2.14 (ident "b")))))
		(annotation @2.1-2.6
			(declared-type
				(ty-fn @1.9-1.21 (effectful false)
					(ty @1.9-1.11 (name "U8"))
					(ty @1.13-1.15 (name "U8"))
					(ty @1.19-1.21 (name "U8"))))))
	(s-expect @4.1-4.24
		(e-binop @4.8-4.24 (op "eq")
			(e-call @4.8-4.19
				(e-lookup-local @4.8-4.13
					(p-assign @2.1-2.6 (ident "addU8")))
				(e-int @4.14-4.15 (value "1"))
				(e-int @4.17-4.18 (value "2")))
			(e-int @4.23-4.24 (value "3"))))
	(s-expect @5.1-5.26
		(e-binop @5.8-5.26 (op "eq")
			(e-call @5.8-5.20
				(e-lookup-local @5.8-5.13
					(p-assign @2.1-2.6 (ident "addU8")))
				(e-int @5.14-5.15 (value "0"))
				(e-int @5.17-5.19 (value "10")))
			(e-int @5.24-5.26 (value "10")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.6 (type "U8, U8 -> U8")))
	(expressions
		(expr @2.9-2.21 (type "U8, U8 -> U8"))))
~~~
