# META
~~~ini
description=Simple demonstration that type variable names avoid collision with existing identifiers
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
    result1 = identity(42)
    result2 = identity2("hello")
    result3 = pair(result1, result2)
    
    a + b + c
}
~~~
# EXPECTED
UNUSED VARIABLE - type_var_collision_simple.md:20:5:20:12
# PROBLEMS
**UNUSED VARIABLE**
Variable `result3` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result3` to suppress this warning.
The unused variable is declared here:
**type_var_collision_simple.md:20:5:20:12:**
```roc
    result3 = pair(result1, result2)
```
    ^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:6),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Int(5:5-5:6),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),Int(6:5-6:6),
LowerIdent(9:1-9:9),OpAssign(9:10-9:11),OpBar(9:12-9:13),LowerIdent(9:13-9:14),OpBar(9:14-9:15),LowerIdent(9:16-9:17),
LowerIdent(12:1-12:10),OpAssign(12:11-12:12),OpBar(12:13-12:14),LowerIdent(12:14-12:15),OpBar(12:15-12:16),LowerIdent(12:17-12:18),
LowerIdent(15:1-15:5),OpAssign(15:6-15:7),OpBar(15:8-15:9),LowerIdent(15:9-15:14),Comma(15:14-15:15),LowerIdent(15:16-15:22),OpBar(15:22-15:23),OpenRound(15:24-15:25),LowerIdent(15:25-15:30),Comma(15:30-15:31),LowerIdent(15:32-15:38),CloseRound(15:38-15:39),
LowerIdent(17:1-17:6),OpAssign(17:7-17:8),OpBar(17:9-17:10),Underscore(17:10-17:11),OpBar(17:11-17:12),OpenCurly(17:13-17:14),
LowerIdent(18:5-18:12),OpAssign(18:13-18:14),LowerIdent(18:15-18:23),NoSpaceOpenRound(18:23-18:24),Int(18:24-18:26),CloseRound(18:26-18:27),
LowerIdent(19:5-19:12),OpAssign(19:13-19:14),LowerIdent(19:15-19:24),NoSpaceOpenRound(19:24-19:25),StringStart(19:25-19:26),StringPart(19:26-19:31),StringEnd(19:31-19:32),CloseRound(19:32-19:33),
LowerIdent(20:5-20:12),OpAssign(20:13-20:14),LowerIdent(20:15-20:19),NoSpaceOpenRound(20:19-20:20),LowerIdent(20:20-20:27),Comma(20:27-20:28),LowerIdent(20:29-20:36),CloseRound(20:36-20:37),
LowerIdent(22:5-22:6),OpPlus(22:7-22:8),LowerIdent(22:9-22:10),OpPlus(22:11-22:12),LowerIdent(22:13-22:14),
CloseCurly(23:1-23:2),
EndOfFile(24:1-24:1),
~~~
# PARSE
~~~clojure
(file @1.1-23.2
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
		(s-decl @4.1-4.6
			(p-ident @4.1-4.2 (raw "a"))
			(e-int @4.5-4.6 (raw "1")))
		(s-decl @5.1-5.6
			(p-ident @5.1-5.2 (raw "b"))
			(e-int @5.5-5.6 (raw "2")))
		(s-decl @6.1-6.6
			(p-ident @6.1-6.2 (raw "c"))
			(e-int @6.5-6.6 (raw "3")))
		(s-decl @9.1-9.17
			(p-ident @9.1-9.9 (raw "identity"))
			(e-lambda @9.12-9.17
				(args
					(p-ident @9.13-9.14 (raw "x")))
				(e-ident @9.16-9.17 (raw "x"))))
		(s-decl @12.1-12.18
			(p-ident @12.1-12.10 (raw "identity2"))
			(e-lambda @12.13-12.18
				(args
					(p-ident @12.14-12.15 (raw "y")))
				(e-ident @12.17-12.18 (raw "y"))))
		(s-decl @15.1-15.39
			(p-ident @15.1-15.5 (raw "pair"))
			(e-lambda @15.8-15.39
				(args
					(p-ident @15.9-15.14 (raw "first"))
					(p-ident @15.16-15.22 (raw "second")))
				(e-tuple @15.24-15.39
					(e-ident @15.25-15.30 (raw "first"))
					(e-ident @15.32-15.38 (raw "second")))))
		(s-decl @17.1-23.2
			(p-ident @17.1-17.6 (raw "main!"))
			(e-lambda @17.9-23.2
				(args
					(p-underscore))
				(e-block @17.13-23.2
					(statements
						(s-decl @18.5-18.27
							(p-ident @18.5-18.12 (raw "result1"))
							(e-apply @18.15-18.27
								(e-ident @18.15-18.23 (raw "identity"))
								(e-int @18.24-18.26 (raw "42"))))
						(s-decl @19.5-19.33
							(p-ident @19.5-19.12 (raw "result2"))
							(e-apply @19.15-19.33
								(e-ident @19.15-19.24 (raw "identity2"))
								(e-string @19.25-19.32
									(e-string-part @19.26-19.31 (raw "hello")))))
						(s-decl @20.5-20.37
							(p-ident @20.5-20.12 (raw "result3"))
							(e-apply @20.15-20.37
								(e-ident @20.15-20.19 (raw "pair"))
								(e-ident @20.20-20.27 (raw "result1"))
								(e-ident @20.29-20.36 (raw "result2"))))
						(e-binop @22.5-22.14 (op "+")
							(e-binop @22.5-22.10 (op "+")
								(e-ident @22.5-22.6 (raw "a"))
								(e-ident @22.9-22.10 (raw "b")))
							(e-ident @22.13-22.14 (raw "c")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Define some variables that would normally be used for type variables
a = 1
b = 2
c = 3

# This identity function should get type 'd -> d' since a, b, c are taken
identity = |x| x

# This function should get type 'e -> e' since d is now also taken
identity2 = |y| y

# This function with two parameters should get types 'f, g -> (f, g)'
pair = |first, second| (first, second)

main! = |_| {
	result1 = identity(42)
	result2 = identity2("hello")
	result3 = pair(result1, result2)

	a + b + c
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "a"))
		(e-int @4.5-4.6 (value "1")))
	(d-let
		(p-assign @5.1-5.2 (ident "b"))
		(e-int @5.5-5.6 (value "2")))
	(d-let
		(p-assign @6.1-6.2 (ident "c"))
		(e-int @6.5-6.6 (value "3")))
	(d-let
		(p-assign @9.1-9.9 (ident "identity"))
		(e-lambda @9.12-9.17
			(args
				(p-assign @9.13-9.14 (ident "x")))
			(e-lookup-local @9.16-9.17
				(p-assign @9.13-9.14 (ident "x")))))
	(d-let
		(p-assign @12.1-12.10 (ident "identity2"))
		(e-lambda @12.13-12.18
			(args
				(p-assign @12.14-12.15 (ident "y")))
			(e-lookup-local @12.17-12.18
				(p-assign @12.14-12.15 (ident "y")))))
	(d-let
		(p-assign @15.1-15.5 (ident "pair"))
		(e-lambda @15.8-15.39
			(args
				(p-assign @15.9-15.14 (ident "first"))
				(p-assign @15.16-15.22 (ident "second")))
			(e-tuple @15.24-15.39
				(elems
					(e-lookup-local @15.25-15.30
						(p-assign @15.9-15.14 (ident "first")))
					(e-lookup-local @15.32-15.38
						(p-assign @15.16-15.22 (ident "second")))))))
	(d-let
		(p-assign @17.1-17.6 (ident "main!"))
		(e-closure @17.9-23.2
			(captures
				(capture @9.1-9.9 (ident "identity"))
				(capture @4.1-4.2 (ident "a"))
				(capture @5.1-5.2 (ident "b"))
				(capture @12.1-12.10 (ident "identity2"))
				(capture @15.1-15.5 (ident "pair"))
				(capture @6.1-6.2 (ident "c")))
			(e-lambda @17.9-23.2
				(args
					(p-underscore @17.10-17.11))
				(e-block @17.13-23.2
					(s-let @18.5-18.27
						(p-assign @18.5-18.12 (ident "result1"))
						(e-call @18.15-18.27
							(e-lookup-local @18.15-18.23
								(p-assign @9.1-9.9 (ident "identity")))
							(e-int @18.24-18.26 (value "42"))))
					(s-let @19.5-19.33
						(p-assign @19.5-19.12 (ident "result2"))
						(e-call @19.15-19.33
							(e-lookup-local @19.15-19.24
								(p-assign @12.1-12.10 (ident "identity2")))
							(e-string @19.25-19.32
								(e-literal @19.26-19.31 (string "hello")))))
					(s-let @20.5-20.37
						(p-assign @20.5-20.12 (ident "result3"))
						(e-call @20.15-20.37
							(e-lookup-local @20.15-20.19
								(p-assign @15.1-15.5 (ident "pair")))
							(e-lookup-local @20.20-20.27
								(p-assign @18.5-18.12 (ident "result1")))
							(e-lookup-local @20.29-20.36
								(p-assign @19.5-19.12 (ident "result2")))))
					(e-binop @22.5-22.14 (op "add")
						(e-binop @22.5-22.10 (op "add")
							(e-lookup-local @22.5-22.6
								(p-assign @4.1-4.2 (ident "a")))
							(e-lookup-local @22.9-22.10
								(p-assign @5.1-5.2 (ident "b"))))
						(e-lookup-local @22.13-22.14
							(p-assign @6.1-6.2 (ident "c")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Num(_size)"))
		(patt @5.1-5.2 (type "Num(_size)"))
		(patt @6.1-6.2 (type "Num(_size)"))
		(patt @9.1-9.9 (type "_arg -> _ret"))
		(patt @12.1-12.10 (type "_arg -> _ret"))
		(patt @15.1-15.5 (type "_arg, _arg2 -> (_field, _field2)"))
		(patt @17.1-17.6 (type "_arg -> Num(_size)")))
	(expressions
		(expr @4.5-4.6 (type "Num(_size)"))
		(expr @5.5-5.6 (type "Num(_size)"))
		(expr @6.5-6.6 (type "Num(_size)"))
		(expr @9.12-9.17 (type "_arg -> _ret"))
		(expr @12.13-12.18 (type "_arg -> _ret"))
		(expr @15.8-15.39 (type "_arg, _arg2 -> (_field, _field2)"))
		(expr @17.9-23.2 (type "_arg -> Num(_size)"))))
~~~
