# META
~~~ini
description=Exact pattern from type_alias_parameterized with variations
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Type alias with parameters, just like the original
Pair(a, b) : (a, b)

# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |(x, y)| (y, x)

# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b), (a -> c), (b -> d) -> Pair(c, d)
map_pair = |(x, y), f, g| (f(x), g(y))

# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
    # This creates Pair(Num, Num)
    p1 = swap_pair((1, 2))

    # This should fail - map_pair expects a tuple but gets four separate arguments
    # And the instantiated types from map_pair should cause issues
    p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))

    p2
}
~~~
# EXPECTED
UNUSED VARIABLE - test_exact_pattern_crash.md:19:5:19:7
TYPE MISMATCH - test_exact_pattern_crash.md:23:10:23:18
# PROBLEMS
**UNUSED VARIABLE**
Variable `p1` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_p1` to suppress this warning.
The unused variable is declared here:
**test_exact_pattern_crash.md:19:5:19:7:**
```roc
    p1 = swap_pair((1, 2))
```
    ^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_exact_pattern_crash.md:23:10:23:18:**
```roc
    p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))
```
         ^^^^^^^^

It is of type:
    _Pair(a, b), Num(_size7) -> Num(_size8), Num(_size9) -> Num(_size10) -> Pair(c, d)_

But you are trying to use it as:
    _Num(_size), Num(_size2), Num(_size3) -> Num(_size4), Num(_size5) -> Num(_size6) -> _ret_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
UpperIdent(4:1-4:5),NoSpaceOpenRound(4:5-4:6),LowerIdent(4:6-4:7),Comma(4:7-4:8),LowerIdent(4:9-4:10),CloseRound(4:10-4:11),OpColon(4:12-4:13),OpenRound(4:14-4:15),LowerIdent(4:15-4:16),Comma(4:16-4:17),LowerIdent(4:18-4:19),CloseRound(4:19-4:20),
LowerIdent(7:1-7:10),OpColon(7:11-7:12),UpperIdent(7:13-7:17),NoSpaceOpenRound(7:17-7:18),LowerIdent(7:18-7:19),Comma(7:19-7:20),LowerIdent(7:21-7:22),CloseRound(7:22-7:23),OpArrow(7:24-7:26),UpperIdent(7:27-7:31),NoSpaceOpenRound(7:31-7:32),LowerIdent(7:32-7:33),Comma(7:33-7:34),LowerIdent(7:35-7:36),CloseRound(7:36-7:37),
LowerIdent(8:1-8:10),OpAssign(8:11-8:12),OpBar(8:13-8:14),NoSpaceOpenRound(8:14-8:15),LowerIdent(8:15-8:16),Comma(8:16-8:17),LowerIdent(8:18-8:19),CloseRound(8:19-8:20),OpBar(8:20-8:21),OpenRound(8:22-8:23),LowerIdent(8:23-8:24),Comma(8:24-8:25),LowerIdent(8:26-8:27),CloseRound(8:27-8:28),
LowerIdent(11:1-11:9),OpColon(11:10-11:11),UpperIdent(11:12-11:16),NoSpaceOpenRound(11:16-11:17),LowerIdent(11:17-11:18),Comma(11:18-11:19),LowerIdent(11:20-11:21),CloseRound(11:21-11:22),Comma(11:22-11:23),OpenRound(11:24-11:25),LowerIdent(11:25-11:26),OpArrow(11:27-11:29),LowerIdent(11:30-11:31),CloseRound(11:31-11:32),Comma(11:32-11:33),OpenRound(11:34-11:35),LowerIdent(11:35-11:36),OpArrow(11:37-11:39),LowerIdent(11:40-11:41),CloseRound(11:41-11:42),OpArrow(11:43-11:45),UpperIdent(11:46-11:50),NoSpaceOpenRound(11:50-11:51),LowerIdent(11:51-11:52),Comma(11:52-11:53),LowerIdent(11:54-11:55),CloseRound(11:55-11:56),
LowerIdent(12:1-12:9),OpAssign(12:10-12:11),OpBar(12:12-12:13),NoSpaceOpenRound(12:13-12:14),LowerIdent(12:14-12:15),Comma(12:15-12:16),LowerIdent(12:17-12:18),CloseRound(12:18-12:19),Comma(12:19-12:20),LowerIdent(12:21-12:22),Comma(12:22-12:23),LowerIdent(12:24-12:25),OpBar(12:25-12:26),OpenRound(12:27-12:28),LowerIdent(12:28-12:29),NoSpaceOpenRound(12:29-12:30),LowerIdent(12:30-12:31),CloseRound(12:31-12:32),Comma(12:32-12:33),LowerIdent(12:34-12:35),NoSpaceOpenRound(12:35-12:36),LowerIdent(12:36-12:37),CloseRound(12:37-12:38),CloseRound(12:38-12:39),
LowerIdent(17:1-17:5),OpAssign(17:6-17:7),OpenCurly(17:8-17:9),
LowerIdent(19:5-19:7),OpAssign(19:8-19:9),LowerIdent(19:10-19:19),NoSpaceOpenRound(19:19-19:20),NoSpaceOpenRound(19:20-19:21),Int(19:21-19:22),Comma(19:22-19:23),Int(19:24-19:25),CloseRound(19:25-19:26),CloseRound(19:26-19:27),
LowerIdent(23:5-23:7),OpAssign(23:8-23:9),LowerIdent(23:10-23:18),NoSpaceOpenRound(23:18-23:19),Int(23:19-23:20),Comma(23:20-23:21),Int(23:22-23:23),Comma(23:23-23:24),OpenRound(23:25-23:26),OpBar(23:26-23:27),LowerIdent(23:27-23:28),OpBar(23:28-23:29),LowerIdent(23:30-23:31),OpPlus(23:32-23:33),Int(23:34-23:35),CloseRound(23:35-23:36),Comma(23:36-23:37),OpenRound(23:38-23:39),OpBar(23:39-23:40),LowerIdent(23:40-23:41),OpBar(23:41-23:42),LowerIdent(23:43-23:44),OpStar(23:45-23:46),Int(23:47-23:48),CloseRound(23:48-23:49),CloseRound(23:49-23:50),
LowerIdent(25:5-25:7),
CloseCurly(26:1-26:2),EndOfFile(26:2-26:2),
~~~
# PARSE
~~~clojure
(file @1.1-26.2
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-decl @4.1-4.20
			(header @4.1-4.11 (name "Pair")
				(args
					(ty-var @4.6-4.7 (raw "a"))
					(ty-var @4.9-4.10 (raw "b"))))
			(ty-tuple @4.14-4.20
				(ty-var @4.15-4.16 (raw "a"))
				(ty-var @4.18-4.19 (raw "b"))))
		(s-type-anno @7.1-7.37 (name "swap_pair")
			(ty-fn @7.13-7.37
				(ty-apply @7.13-7.23
					(ty @7.13-7.17 (name "Pair"))
					(ty-var @7.18-7.19 (raw "a"))
					(ty-var @7.21-7.22 (raw "b")))
				(ty-apply @7.27-7.37
					(ty @7.27-7.31 (name "Pair"))
					(ty-var @7.32-7.33 (raw "b"))
					(ty-var @7.35-7.36 (raw "a")))))
		(s-decl @8.1-8.28
			(p-ident @8.1-8.10 (raw "swap_pair"))
			(e-lambda @8.13-8.28
				(args
					(p-tuple @8.14-8.20
						(p-ident @8.15-8.16 (raw "x"))
						(p-ident @8.18-8.19 (raw "y"))))
				(e-tuple @8.22-8.28
					(e-ident @8.23-8.24 (raw "y"))
					(e-ident @8.26-8.27 (raw "x")))))
		(s-type-anno @11.1-11.56 (name "map_pair")
			(ty-fn @11.12-11.56
				(ty-apply @11.12-11.22
					(ty @11.12-11.16 (name "Pair"))
					(ty-var @11.17-11.18 (raw "a"))
					(ty-var @11.20-11.21 (raw "b")))
				(ty-fn @11.25-11.31
					(ty-var @11.25-11.26 (raw "a"))
					(ty-var @11.30-11.31 (raw "c")))
				(ty-fn @11.35-11.41
					(ty-var @11.35-11.36 (raw "b"))
					(ty-var @11.40-11.41 (raw "d")))
				(ty-apply @11.46-11.56
					(ty @11.46-11.50 (name "Pair"))
					(ty-var @11.51-11.52 (raw "c"))
					(ty-var @11.54-11.55 (raw "d")))))
		(s-decl @12.1-12.39
			(p-ident @12.1-12.9 (raw "map_pair"))
			(e-lambda @12.12-12.39
				(args
					(p-tuple @12.13-12.19
						(p-ident @12.14-12.15 (raw "x"))
						(p-ident @12.17-12.18 (raw "y")))
					(p-ident @12.21-12.22 (raw "f"))
					(p-ident @12.24-12.25 (raw "g")))
				(e-tuple @12.27-12.39
					(e-apply @12.28-12.32
						(e-ident @12.28-12.29 (raw "f"))
						(e-ident @12.30-12.31 (raw "x")))
					(e-apply @12.34-12.38
						(e-ident @12.34-12.35 (raw "g"))
						(e-ident @12.36-12.37 (raw "y"))))))
		(s-decl @17.1-26.2
			(p-ident @17.1-17.5 (raw "main"))
			(e-block @17.8-26.2
				(statements
					(s-decl @19.5-19.27
						(p-ident @19.5-19.7 (raw "p1"))
						(e-apply @19.10-19.27
							(e-ident @19.10-19.19 (raw "swap_pair"))
							(e-tuple @19.20-19.26
								(e-int @19.21-19.22 (raw "1"))
								(e-int @19.24-19.25 (raw "2")))))
					(s-decl @23.5-23.50
						(p-ident @23.5-23.7 (raw "p2"))
						(e-apply @23.10-23.50
							(e-ident @23.10-23.18 (raw "map_pair"))
							(e-int @23.19-23.20 (raw "3"))
							(e-int @23.22-23.23 (raw "4"))
							(e-tuple @23.25-23.36
								(e-lambda @23.26-23.35
									(args
										(p-ident @23.27-23.28 (raw "x")))
									(e-binop @23.30-23.35 (op "+")
										(e-ident @23.30-23.31 (raw "x"))
										(e-int @23.34-23.35 (raw "1")))))
							(e-tuple @23.38-23.49
								(e-lambda @23.39-23.48
									(args
										(p-ident @23.40-23.41 (raw "y")))
									(e-binop @23.43-23.48 (op "*")
										(e-ident @23.43-23.44 (raw "y"))
										(e-int @23.47-23.48 (raw "2")))))))
					(e-ident @25.5-25.7 (raw "p2")))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Type alias with parameters, just like the original
Pair(a, b) : (a, b)

# Function that uses the alias and will need instantiation
swap_pair : Pair(a, b) -> Pair(b, a)
swap_pair = |(x, y)| (y, x)

# Another polymorphic function to create more complex instantiation
map_pair : Pair(a, b), (a -> c), (b -> d) -> Pair(c, d)
map_pair = |(x, y), f, g| (f(x), g(y))

# This should trigger multiple instantiations
# First swap_pair gets instantiated, then map_pair
# The error should involve deeply nested instantiated types
main = {
	# This creates Pair(Num, Num)
	p1 = swap_pair((1, 2))

	# This should fail - map_pair expects a tuple but gets four separate arguments
	# And the instantiated types from map_pair should cause issues
	p2 = map_pair(3, 4, (|x| x + 1), (|y| y * 2))

	p2
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @8.1-8.10 (ident "swap_pair")))
		(expr
			(e-lambda @8.13-8.28
				(args
					(p-tuple @8.14-8.20
						(patterns
							(p-assign @8.15-8.16 (ident "x"))
							(p-assign @8.18-8.19 (ident "y")))))
				(e-tuple @8.22-8.28
					(elems
						(e-lookup-local @8.23-8.24
							(p-assign @8.18-8.19 (ident "y")))
						(e-lookup-local @8.26-8.27
							(p-assign @8.15-8.16 (ident "x")))))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @7.13-7.37 (effectful false)
						(ty-apply @7.13-7.23 (symbol "Pair")
							(ty-var @7.18-7.19 (name "a"))
							(ty-var @7.21-7.22 (name "b")))
						(ty-apply @7.27-7.37 (symbol "Pair")
							(ty-var @7.32-7.33 (name "b"))
							(ty-var @7.35-7.36 (name "a"))))))))
	(def
		(pattern
			(p-assign @12.1-12.9 (ident "map_pair")))
		(expr
			(e-lambda @12.12-12.39
				(args
					(p-tuple @12.13-12.19
						(patterns
							(p-assign @12.14-12.15 (ident "x"))
							(p-assign @12.17-12.18 (ident "y"))))
					(p-assign @12.21-12.22 (ident "f"))
					(p-assign @12.24-12.25 (ident "g")))
				(e-tuple @12.27-12.39
					(elems
						(e-call @12.28-12.32
							(e-lookup-local @12.28-12.29
								(p-assign @12.21-12.22 (ident "f")))
							(e-lookup-local @12.30-12.31
								(p-assign @12.14-12.15 (ident "x"))))
						(e-call @12.34-12.38
							(e-lookup-local @12.34-12.35
								(p-assign @12.24-12.25 (ident "g")))
							(e-lookup-local @12.36-12.37
								(p-assign @12.17-12.18 (ident "y"))))))))
		(annotation
			(annotation
				(type-anno
					(ty-fn @11.12-11.56 (effectful false)
						(ty-apply @11.12-11.22 (symbol "Pair")
							(ty-var @11.17-11.18 (name "a"))
							(ty-var @11.20-11.21 (name "b")))
						(ty-parens @11.24-11.32
							(ty-fn @11.25-11.31 (effectful false)
								(ty-var @11.25-11.26 (name "a"))
								(ty-var @11.30-11.31 (name "c"))))
						(ty-parens @11.34-11.42
							(ty-fn @11.35-11.41 (effectful false)
								(ty-var @11.35-11.36 (name "b"))
								(ty-var @11.40-11.41 (name "d"))))
						(ty-apply @11.46-11.56 (symbol "Pair")
							(ty-var @11.51-11.52 (name "c"))
							(ty-var @11.54-11.55 (name "d"))))))))
	(def
		(pattern
			(p-assign @17.1-17.5 (ident "main")))
		(expr
			(e-block @17.8-26.2
				(s-let @19.5-19.27
					(p-assign @19.5-19.7 (ident "p1"))
					(e-call @19.10-19.27
						(e-lookup-local @19.10-19.19
							(p-assign @8.1-8.10 (ident "swap_pair")))
						(e-tuple @19.20-19.26
							(elems
								(e-int @19.21-19.22 (value "1"))
								(e-int @19.24-19.25 (value "2"))))))
				(s-let @23.5-23.50
					(p-assign @23.5-23.7 (ident "p2"))
					(e-call @23.10-23.50
						(e-lookup-local @23.10-23.18
							(p-assign @12.1-12.9 (ident "map_pair")))
						(e-int @23.19-23.20 (value "3"))
						(e-int @23.22-23.23 (value "4"))
						(e-lambda @23.26-23.35
							(args
								(p-assign @23.27-23.28 (ident "x")))
							(e-binop @23.30-23.35 (op "add")
								(e-lookup-local @23.30-23.31
									(p-assign @23.27-23.28 (ident "x")))
								(e-int @23.34-23.35 (value "1"))))
						(e-lambda @23.39-23.48
							(args
								(p-assign @23.40-23.41 (ident "y")))
							(e-binop @23.43-23.48 (op "mul")
								(e-lookup-local @23.43-23.44
									(p-assign @23.40-23.41 (ident "y")))
								(e-int @23.47-23.48 (value "2"))))))
				(e-lookup-local @25.5-25.7
					(p-assign @23.5-23.7 (ident "p2"))))))
	(s-alias-decl @4.1-4.20
		(type-header (name "Pair")
			(args
				(ty-var @4.6-4.7 (name "a"))
				(ty-var @4.9-4.10 (name "b"))))
		(ty-tuple @4.14-4.20
			(ty-var @4.15-4.16 (name "a"))
			(ty-var @4.18-4.19 (name "b")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.10 (type "Pair(a, b) -> Pair(b, a)"))
		(patt @12.1-12.9 (type "Error"))
		(patt @17.1-17.5 (type "_e")))
	(type_decls
		(alias @4.1-4.20 (type "Pair(a, b)")
			(type-header (name "Pair")
				(args
					(ty-var @4.6-4.7 (name "a"))
					(ty-var @4.9-4.10 (name "b"))))))
	(expressions
		(expr @8.13-8.28 (type "Pair(a, b) -> Pair(b, a)"))
		(expr @12.12-12.39 (type "Error"))
		(expr @17.8-26.2 (type "_e"))))
~~~
