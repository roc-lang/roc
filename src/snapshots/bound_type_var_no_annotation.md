# META
~~~ini
description=A bound type variable (for identity function) with no type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
Variable ``pair`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:
**bound_type_var_no_annotation.md:19:5:19:9:**
```roc
    pair = combine(num, text)
```
    ^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:9),OpAssign(3:10-3:11),OpBar(3:12-3:13),LowerIdent(3:13-3:14),OpBar(3:14-3:15),LowerIdent(3:16-3:17),
LowerIdent(6:1-6:8),OpColon(6:9-6:10),LowerIdent(6:11-6:12),Comma(6:12-6:13),LowerIdent(6:14-6:15),OpArrow(6:16-6:18),OpenRound(6:19-6:20),LowerIdent(6:20-6:21),Comma(6:21-6:22),LowerIdent(6:23-6:24),CloseRound(6:24-6:25),
LowerIdent(7:1-7:8),OpAssign(7:9-7:10),OpBar(7:11-7:12),LowerIdent(7:12-7:17),Comma(7:17-7:18),LowerIdent(7:19-7:25),OpBar(7:25-7:26),OpenRound(7:27-7:28),LowerIdent(7:28-7:33),Comma(7:33-7:34),LowerIdent(7:35-7:41),CloseRound(7:41-7:42),
LowerIdent(10:1-10:7),OpColon(10:8-10:9),UpperIdent(10:10-10:13),OpArrow(10:14-10:16),UpperIdent(10:17-10:20),
LowerIdent(11:1-11:7),OpAssign(11:8-11:9),OpBar(11:10-11:11),LowerIdent(11:11-11:12),OpBar(11:12-11:13),LowerIdent(11:14-11:15),OpPlus(11:16-11:17),Int(11:18-11:19),
LowerIdent(13:1-13:6),OpAssign(13:7-13:8),OpBar(13:9-13:10),Underscore(13:10-13:11),OpBar(13:11-13:12),OpenCurly(13:13-13:14),
LowerIdent(15:5-15:8),OpAssign(15:9-15:10),LowerIdent(15:11-15:19),NoSpaceOpenRound(15:19-15:20),Int(15:20-15:22),CloseRound(15:22-15:23),
LowerIdent(16:5-16:9),OpAssign(16:10-16:11),LowerIdent(16:12-16:20),NoSpaceOpenRound(16:20-16:21),StringStart(16:21-16:22),StringPart(16:22-16:27),StringEnd(16:27-16:28),CloseRound(16:28-16:29),
LowerIdent(19:5-19:9),OpAssign(19:10-19:11),LowerIdent(19:12-19:19),NoSpaceOpenRound(19:19-19:20),LowerIdent(19:20-19:23),Comma(19:23-19:24),LowerIdent(19:25-19:29),CloseRound(19:29-19:30),
LowerIdent(22:5-22:11),OpAssign(22:12-22:13),LowerIdent(22:14-22:20),NoSpaceOpenRound(22:20-22:21),Int(22:21-22:22),CloseRound(22:22-22:23),
LowerIdent(24:5-24:11),
CloseCurly(25:1-25:2),EndOfFile(25:2-25:2),
~~~
# PARSE
~~~clojure
(file @1.1-25.2
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11 (text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-decl @3.1-3.17
			(p-ident @3.1-3.9 (raw "identity"))
			(e-lambda @3.12-3.17
				(args
					(p-ident @3.13-3.14 (raw "x")))
				(e-ident @3.16-3.17 (raw "x"))))
		(s-type-anno @6.1-6.25 (name "combine")
			(ty-fn @6.11-6.25
				(ty-var @1.1-1.1 (raw "a"))
				(ty-var @1.1-1.1 (raw "b"))
				(ty-tuple @6.19-6.25
					(ty-var @6.20-6.20 (raw "a"))
					(ty-var @1.1-1.1 (raw "b")))))
		(s-decl @7.1-7.42
			(p-ident @7.1-7.8 (raw "combine"))
			(e-lambda @7.11-7.42
				(args
					(p-ident @7.12-7.17 (raw "first"))
					(p-ident @7.19-7.25 (raw "second")))
				(e-tuple @7.27-7.42
					(e-ident @7.28-7.33 (raw "first"))
					(e-ident @7.35-7.41 (raw "second")))))
		(s-type-anno @10.1-10.20 (name "addOne")
			(ty-fn @10.10-10.20
				(ty @10.10-10.13 (name "U64"))
				(ty @10.17-10.20 (name "U64"))))
		(s-decl @11.1-11.19
			(p-ident @11.1-11.7 (raw "addOne"))
			(e-lambda @11.10-11.19
				(args
					(p-ident @11.11-11.12 (raw "n")))
				(e-binop @11.14-11.19 (op "+")
					(e-ident @11.14-11.15 (raw "n"))
					(e-int @11.18-11.19 (raw "1")))))
		(s-decl @13.1-25.2
			(p-ident @13.1-13.6 (raw "main!"))
			(e-lambda @13.9-25.2
				(args
					(p-underscore))
				(e-block @13.13-25.2
					(statements
						(s-decl @15.5-15.23
							(p-ident @15.5-15.8 (raw "num"))
							(e-apply @15.11-15.23
								(e-ident @15.11-15.19 (raw "identity"))
								(e-int @15.20-15.22 (raw "42"))))
						(s-decl @16.5-16.29
							(p-ident @16.5-16.9 (raw "text"))
							(e-apply @16.12-16.29
								(e-ident @16.12-16.20 (raw "identity"))
								(e-string @16.21-16.28
									(e-string-part @16.22-16.27 (raw "hello")))))
						(s-decl @19.5-19.30
							(p-ident @19.5-19.9 (raw "pair"))
							(e-apply @19.12-19.30
								(e-ident @19.12-19.19 (raw "combine"))
								(e-ident @19.20-19.23 (raw "num"))
								(e-ident @19.25-19.29 (raw "text"))))
						(s-decl @22.5-22.23
							(p-ident @22.5-22.11 (raw "result"))
							(e-apply @22.14-22.23
								(e-ident @22.14-22.20 (raw "addOne"))
								(e-int @22.21-22.22 (raw "5"))))
						(e-ident @24.5-24.11 (raw "result"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

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
		(p-assign @3.1-3.9 (ident "identity"))
		(e-lambda @3.12-3.17
			(args
				(p-assign @3.13-3.14 (ident "x")))
			(e-lookup-local @3.16-3.17
				(p-assign @3.13-3.14 (ident "x")))))
	(d-let
		(p-assign @7.1-7.8 (ident "combine"))
		(e-lambda @7.11-7.42
			(args
				(p-assign @7.12-7.17 (ident "first"))
				(p-assign @7.19-7.25 (ident "second")))
			(e-tuple @7.27-7.42
				(elems
					(e-lookup-local @7.28-7.33
						(p-assign @7.12-7.17 (ident "first")))
					(e-lookup-local @7.35-7.41
						(p-assign @7.19-7.25 (ident "second"))))))
		(annotation @7.1-7.8
			(declared-type
				(ty-fn @6.11-6.25 (effectful false)
					(ty-var @1.1-1.1 (name "a"))
					(ty-var @1.1-1.1 (name "b"))
					(ty-tuple @6.19-6.25
						(ty-var @6.20-6.20 (name "a"))
						(ty-var @1.1-1.1 (name "b")))))))
	(d-let
		(p-assign @11.1-11.7 (ident "addOne"))
		(e-lambda @11.10-11.19
			(args
				(p-assign @11.11-11.12 (ident "n")))
			(e-binop @11.14-11.19 (op "add")
				(e-lookup-local @11.14-11.15
					(p-assign @11.11-11.12 (ident "n")))
				(e-int @11.18-11.19 (value "1"))))
		(annotation @11.1-11.7
			(declared-type
				(ty-fn @10.10-10.20 (effectful false)
					(ty @10.10-10.13 (name "U64"))
					(ty @10.17-10.20 (name "U64"))))))
	(d-let
		(p-assign @13.1-13.6 (ident "main!"))
		(e-lambda @13.9-25.2
			(args
				(p-underscore @13.10-13.11))
			(e-block @13.13-25.2
				(s-let @15.5-15.23
					(p-assign @15.5-15.8 (ident "num"))
					(e-call @15.11-15.23
						(e-lookup-local @15.11-15.19
							(p-assign @3.1-3.9 (ident "identity")))
						(e-int @15.20-15.22 (value "42"))))
				(s-let @16.5-16.29
					(p-assign @16.5-16.9 (ident "text"))
					(e-call @16.12-16.29
						(e-lookup-local @16.12-16.20
							(p-assign @3.1-3.9 (ident "identity")))
						(e-string @16.21-16.28
							(e-literal @16.22-16.27 (string "hello")))))
				(s-let @19.5-19.30
					(p-assign @19.5-19.9 (ident "pair"))
					(e-call @19.12-19.30
						(e-lookup-local @19.12-19.19
							(p-assign @7.1-7.8 (ident "combine")))
						(e-lookup-local @19.20-19.23
							(p-assign @15.5-15.8 (ident "num")))
						(e-lookup-local @19.25-19.29
							(p-assign @16.5-16.9 (ident "text")))))
				(s-let @22.5-22.23
					(p-assign @22.5-22.11 (ident "result"))
					(e-call @22.14-22.23
						(e-lookup-local @22.14-22.20
							(p-assign @11.1-11.7 (ident "addOne")))
						(e-int @22.21-22.22 (value "5"))))
				(e-lookup-local @24.5-24.11
					(p-assign @22.5-22.11 (ident "result")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.9 (type "a -> b"))
		(patt @7.1-7.8 (type "a, b -> (a, b)"))
		(patt @11.1-11.7 (type "U64 -> U64"))
		(patt @13.1-13.6 (type "a -> U64")))
	(expressions
		(expr @3.12-3.17 (type "a -> b"))
		(expr @7.11-7.42 (type "a, b -> (a, b)"))
		(expr @11.10-11.19 (type "U64 -> U64"))
		(expr @13.9-25.2 (type "a -> U64"))))
~~~
