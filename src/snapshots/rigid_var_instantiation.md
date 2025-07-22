# META
~~~ini
description=Polymorphic function with rigid type variable used at multiple call sites
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
    # First call with number
    num = identity(42)
    
    # Second call with string
    str = identity("hello")
    
    # Third call with list
    lst = identity([1, 2, 3])
    
    {}
}
~~~
# EXPECTED
UNUSED VARIABLE - rigid_var_instantiation.md:13:5:13:8
UNUSED VARIABLE - rigid_var_instantiation.md:10:5:10:8
UNUSED VARIABLE - rigid_var_instantiation.md:16:5:16:8
# PROBLEMS
**UNUSED VARIABLE**
Variable `str` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_str` to suppress this warning.
The unused variable is declared here:
**rigid_var_instantiation.md:13:5:13:8:**
```roc
    str = identity("hello")
```
    ^^^


**UNUSED VARIABLE**
Variable `num` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_num` to suppress this warning.
The unused variable is declared here:
**rigid_var_instantiation.md:10:5:10:8:**
```roc
    num = identity(42)
```
    ^^^


**UNUSED VARIABLE**
Variable `lst` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_lst` to suppress this warning.
The unused variable is declared here:
**rigid_var_instantiation.md:16:5:16:8:**
```roc
    lst = identity([1, 2, 3])
```
    ^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
LowerIdent(4:1-4:9),OpColon(4:10-4:11),LowerIdent(4:12-4:13),OpArrow(4:14-4:16),LowerIdent(4:17-4:18),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpBar(5:12-5:13),LowerIdent(5:13-5:14),OpBar(5:14-5:15),LowerIdent(5:16-5:17),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),OpenCurly(8:13-8:14),
LowerIdent(10:5-10:8),OpAssign(10:9-10:10),LowerIdent(10:11-10:19),NoSpaceOpenRound(10:19-10:20),Int(10:20-10:22),CloseRound(10:22-10:23),
LowerIdent(13:5-13:8),OpAssign(13:9-13:10),LowerIdent(13:11-13:19),NoSpaceOpenRound(13:19-13:20),StringStart(13:20-13:21),StringPart(13:21-13:26),StringEnd(13:26-13:27),CloseRound(13:27-13:28),
LowerIdent(16:5-16:8),OpAssign(16:9-16:10),LowerIdent(16:11-16:19),NoSpaceOpenRound(16:19-16:20),OpenSquare(16:20-16:21),Int(16:21-16:22),Comma(16:22-16:23),Int(16:24-16:25),Comma(16:25-16:26),Int(16:27-16:28),CloseSquare(16:28-16:29),CloseRound(16:29-16:30),
OpenCurly(18:5-18:6),CloseCurly(18:6-18:7),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(file @1.1-19.2
	(app @1.1-1.57
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.55 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.55 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
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
		(s-decl @8.1-19.2
			(p-ident @8.1-8.6 (raw "main!"))
			(e-lambda @8.9-19.2
				(args
					(p-underscore))
				(e-block @8.13-19.2
					(statements
						(s-decl @10.5-10.23
							(p-ident @10.5-10.8 (raw "num"))
							(e-apply @10.11-10.23
								(e-ident @10.11-10.19 (raw "identity"))
								(e-int @10.20-10.22 (raw "42"))))
						(s-decl @13.5-13.28
							(p-ident @13.5-13.8 (raw "str"))
							(e-apply @13.11-13.28
								(e-ident @13.11-13.19 (raw "identity"))
								(e-string @13.20-13.27
									(e-string-part @13.21-13.26 (raw "hello")))))
						(s-decl @16.5-16.30
							(p-ident @16.5-16.8 (raw "lst"))
							(e-apply @16.11-16.30
								(e-ident @16.11-16.19 (raw "identity"))
								(e-list @16.20-16.29
									(e-int @16.21-16.22 (raw "1"))
									(e-int @16.24-16.25 (raw "2"))
									(e-int @16.27-16.28 (raw "3")))))
						(e-record @18.5-18.7)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Polymorphic identity function with rigid type variable 'a'
identity : a -> a
identity = |x| x

# Use identity at different call sites with different types
main! = |_| {
	# First call with number
	num = identity(42)

	# Second call with string
	str = identity("hello")

	# Third call with list
	lst = identity([1, 2, 3])

	{}
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
		(p-assign @8.1-8.6 (ident "main!"))
		(e-lambda @8.9-19.2
			(args
				(p-underscore @8.10-8.11))
			(captures
				(capture (name "identity")))
			(e-block @8.13-19.2
				(s-let @10.5-10.23
					(p-assign @10.5-10.8 (ident "num"))
					(e-call @10.11-10.23
						(e-lookup-local @10.11-10.19
							(p-assign @5.1-5.9 (ident "identity")))
						(e-int @10.20-10.22 (value "42"))))
				(s-let @13.5-13.28
					(p-assign @13.5-13.8 (ident "str"))
					(e-call @13.11-13.28
						(e-lookup-local @13.11-13.19
							(p-assign @5.1-5.9 (ident "identity")))
						(e-string @13.20-13.27
							(e-literal @13.21-13.26 (string "hello")))))
				(s-let @16.5-16.30
					(p-assign @16.5-16.8 (ident "lst"))
					(e-call @16.11-16.30
						(e-lookup-local @16.11-16.19
							(p-assign @5.1-5.9 (ident "identity")))
						(e-list @16.20-16.29
							(elems
								(e-int @16.21-16.22 (value "1"))
								(e-int @16.24-16.25 (value "2"))
								(e-int @16.27-16.28 (value "3"))))))
				(e-empty_record @18.5-18.7)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.9 (type "a -> a"))
		(patt @8.1-8.6 (type "_arg -> {}")))
	(expressions
		(expr @5.12-5.17 (type "a -> a"))
		(expr @8.9-19.2 (type "_arg -> {}"))))
~~~
