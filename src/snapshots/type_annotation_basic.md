# META
~~~ini
description=Basic type annotations with type variables and application
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test generic identity function
identity : a -> a
identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
    # Test identity with different types
    num = identity(42)
    text = identity("hello")

    # Test combine function
    pair = combine(num, text)

    # Test concrete function
    result = addOne(5)

    result
}
~~~
# EXPECTED
UNUSED VARIABLE - type_annotation_basic.md:21:5:21:9
# PROBLEMS
**UNUSED VARIABLE**
Variable `pair` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair to suppress this warning.
The unused variable is declared here:
**type_annotation_basic.md:21:5:21:9:**
```roc
    pair = combine(num, text)
```
    ^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:9),OpColon(4:10-4:11),LowerIdent(4:12-4:13),OpArrow(4:14-4:16),LowerIdent(4:17-4:18),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpBar(5:12-5:13),LowerIdent(5:13-5:14),OpBar(5:14-5:15),LowerIdent(5:16-5:17),
LowerIdent(8:1-8:8),OpColon(8:9-8:10),LowerIdent(8:11-8:12),Comma(8:12-8:13),LowerIdent(8:14-8:15),OpArrow(8:16-8:18),OpenRound(8:19-8:20),LowerIdent(8:20-8:21),Comma(8:21-8:22),LowerIdent(8:23-8:24),CloseRound(8:24-8:25),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),OpBar(9:11-9:12),LowerIdent(9:12-9:17),Comma(9:17-9:18),LowerIdent(9:19-9:25),OpBar(9:25-9:26),OpenRound(9:27-9:28),LowerIdent(9:28-9:33),Comma(9:33-9:34),LowerIdent(9:35-9:41),CloseRound(9:41-9:42),
LowerIdent(12:1-12:7),OpColon(12:8-12:9),UpperIdent(12:10-12:13),OpArrow(12:14-12:16),UpperIdent(12:17-12:20),
LowerIdent(13:1-13:7),OpAssign(13:8-13:9),OpBar(13:10-13:11),LowerIdent(13:11-13:12),OpBar(13:12-13:13),LowerIdent(13:14-13:15),OpPlus(13:16-13:17),Int(13:18-13:19),
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),OpenCurly(15:13-15:14),
LowerIdent(17:5-17:8),OpAssign(17:9-17:10),LowerIdent(17:11-17:19),NoSpaceOpenRound(17:19-17:20),Int(17:20-17:22),CloseRound(17:22-17:23),
LowerIdent(18:5-18:9),OpAssign(18:10-18:11),LowerIdent(18:12-18:20),NoSpaceOpenRound(18:20-18:21),StringStart(18:21-18:22),StringPart(18:22-18:27),StringEnd(18:27-18:28),CloseRound(18:28-18:29),
LowerIdent(21:5-21:9),OpAssign(21:10-21:11),LowerIdent(21:12-21:19),NoSpaceOpenRound(21:19-21:20),LowerIdent(21:20-21:23),Comma(21:23-21:24),LowerIdent(21:25-21:29),CloseRound(21:29-21:30),
LowerIdent(24:5-24:11),OpAssign(24:12-24:13),LowerIdent(24:14-24:20),NoSpaceOpenRound(24:20-24:21),Int(24:21-24:22),CloseRound(24:22-24:23),
LowerIdent(26:5-26:11),
CloseCurly(27:1-27:2),EndOfFile(27:2-27:2),
~~~
# PARSE
~~~clojure
(file @1.1-27.2
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @4.1-4.18 (name "identity")
			(ty-fn @4.12-4.18
				(ty-var @4.12-4.13 (raw "a"))
				(ty-var @4.17-4.18 (raw "a"))))
		(s-decl @5.1-5.17
			(p-ident @5.1-5.9 (raw "identity"))
			(e-lambda @5.12-5.17
				(args
					(p-ident @5.13-5.14 (raw "x")))
				(e-ident @5.16-5.17 (raw "x"))))
		(s-type-anno @8.1-8.25 (name "combine")
			(ty-fn @8.11-8.25
				(ty-var @8.11-8.12 (raw "a"))
				(ty-var @8.14-8.15 (raw "b"))
				(ty-tuple @8.19-8.25
					(ty-var @8.20-8.21 (raw "a"))
					(ty-var @8.23-8.24 (raw "b")))))
		(s-decl @9.1-9.42
			(p-ident @9.1-9.8 (raw "combine"))
			(e-lambda @9.11-9.42
				(args
					(p-ident @9.12-9.17 (raw "first"))
					(p-ident @9.19-9.25 (raw "second")))
				(e-tuple @9.27-9.42
					(e-ident @9.28-9.33 (raw "first"))
					(e-ident @9.35-9.41 (raw "second")))))
		(s-type-anno @12.1-12.20 (name "addOne")
			(ty-fn @12.10-12.20
				(ty @12.10-12.13 (name "U64"))
				(ty @12.17-12.20 (name "U64"))))
		(s-decl @13.1-13.19
			(p-ident @13.1-13.7 (raw "addOne"))
			(e-lambda @13.10-13.19
				(args
					(p-ident @13.11-13.12 (raw "n")))
				(e-binop @13.14-13.19 (op "+")
					(e-ident @13.14-13.15 (raw "n"))
					(e-int @13.18-13.19 (raw "1")))))
		(s-decl @15.1-27.2
			(p-ident @15.1-15.6 (raw "main!"))
			(e-lambda @15.9-27.2
				(args
					(p-underscore))
				(e-block @15.13-27.2
					(statements
						(s-decl @17.5-17.23
							(p-ident @17.5-17.8 (raw "num"))
							(e-apply @17.11-17.23
								(e-ident @17.11-17.19 (raw "identity"))
								(e-int @17.20-17.22 (raw "42"))))
						(s-decl @18.5-18.29
							(p-ident @18.5-18.9 (raw "text"))
							(e-apply @18.12-18.29
								(e-ident @18.12-18.20 (raw "identity"))
								(e-string @18.21-18.28
									(e-string-part @18.22-18.27 (raw "hello")))))
						(s-decl @21.5-21.30
							(p-ident @21.5-21.9 (raw "pair"))
							(e-apply @21.12-21.30
								(e-ident @21.12-21.19 (raw "combine"))
								(e-ident @21.20-21.23 (raw "num"))
								(e-ident @21.25-21.29 (raw "text"))))
						(s-decl @24.5-24.23
							(p-ident @24.5-24.11 (raw "result"))
							(e-apply @24.14-24.23
								(e-ident @24.14-24.20 (raw "addOne"))
								(e-int @24.21-24.22 (raw "5"))))
						(e-ident @26.5-26.11 (raw "result"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Test generic identity function
identity : a -> a
identity = |x| x

# Test function with multiple type parameters
combine : a, b -> (a, b)
combine = |first, second| (first, second)

# Test type application with concrete types
addOne : U64 -> U64
addOne = |n| n + 1

main! = |_| {
	# Test identity with different types
	num = identity(42)
	text = identity("hello")

	# Test combine function
	pair = combine(num, text)

	# Test concrete function
	result = addOne(5)

	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.9 (ident "identity"))
		(e-lambda @5.12-5.17
			(args
				(p-assign @5.13-5.14 (ident "x")))
			(e-lookup-local @5.16-5.17
				(p-assign @5.13-5.14 (ident "x"))))
		(annotation @5.1-5.9
			(declared-type
				(ty-fn @4.12-4.18 (effectful false)
					(ty-var @4.12-4.13 (name "a"))
					(ty-var @4.17-4.18 (name "a"))))))
	(d-let
		(p-assign @9.1-9.8 (ident "combine"))
		(e-lambda @9.11-9.42
			(args
				(p-assign @9.12-9.17 (ident "first"))
				(p-assign @9.19-9.25 (ident "second")))
			(e-tuple @9.27-9.42
				(elems
					(e-lookup-local @9.28-9.33
						(p-assign @9.12-9.17 (ident "first")))
					(e-lookup-local @9.35-9.41
						(p-assign @9.19-9.25 (ident "second"))))))
		(annotation @9.1-9.8
			(declared-type
				(ty-fn @8.11-8.25 (effectful false)
					(ty-var @8.11-8.12 (name "a"))
					(ty-var @8.14-8.15 (name "b"))
					(ty-tuple @8.19-8.25
						(ty-var @8.20-8.21 (name "a"))
						(ty-var @8.23-8.24 (name "b")))))))
	(d-let
		(p-assign @13.1-13.7 (ident "addOne"))
		(e-lambda @13.10-13.19
			(args
				(p-assign @13.11-13.12 (ident "n")))
			(e-binop @13.14-13.19 (op "add")
				(e-lookup-local @13.14-13.15
					(p-assign @13.11-13.12 (ident "n")))
				(e-int @13.18-13.19 (value "1"))))
		(annotation @13.1-13.7
			(declared-type
				(ty-fn @12.10-12.20 (effectful false)
					(ty @12.10-12.13 (name "U64"))
					(ty @12.17-12.20 (name "U64"))))))
	(d-let
		(p-assign @15.1-15.6 (ident "main!"))
		(e-lambda @15.9-27.2
			(args
				(p-underscore @15.10-15.11))
			(e-block @15.13-27.2
				(s-let @17.5-17.23
					(p-assign @17.5-17.8 (ident "num"))
					(e-call @17.11-17.23
						(e-lookup-local @17.11-17.19
							(p-assign @5.1-5.9 (ident "identity")))
						(e-int @17.20-17.22 (value "42"))))
				(s-let @18.5-18.29
					(p-assign @18.5-18.9 (ident "text"))
					(e-call @18.12-18.29
						(e-lookup-local @18.12-18.20
							(p-assign @5.1-5.9 (ident "identity")))
						(e-string @18.21-18.28
							(e-literal @18.22-18.27 (string "hello")))))
				(s-let @21.5-21.30
					(p-assign @21.5-21.9 (ident "pair"))
					(e-call @21.12-21.30
						(e-lookup-local @21.12-21.19
							(p-assign @9.1-9.8 (ident "combine")))
						(e-lookup-local @21.20-21.23
							(p-assign @17.5-17.8 (ident "num")))
						(e-lookup-local @21.25-21.29
							(p-assign @18.5-18.9 (ident "text")))))
				(s-let @24.5-24.23
					(p-assign @24.5-24.11 (ident "result"))
					(e-call @24.14-24.23
						(e-lookup-local @24.14-24.20
							(p-assign @13.1-13.7 (ident "addOne")))
						(e-int @24.21-24.22 (value "5"))))
				(e-lookup-local @26.5-26.11
					(p-assign @24.5-24.11 (ident "result")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.9 (type "a -> a"))
		(patt @9.1-9.8 (type "a, b -> (a, b)"))
		(patt @13.1-13.7 (type "U64 -> U64"))
		(patt @15.1-15.6 (type "_arg -> U64")))
	(expressions
		(expr @5.12-5.17 (type "a -> a"))
		(expr @9.11-9.42 (type "a, b -> (a, b)"))
		(expr @13.10-13.19 (type "U64 -> U64"))
		(expr @15.9-27.2 (type "_arg -> U64"))))
~~~
