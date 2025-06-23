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
# PROBLEMS
**UNUSED VARIABLE**
Variable ``pair`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:
**type_annotation_basic.md:21:5:21:9:**
```roc
    pair = combine(num, text)
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:33),
LowerIdent(4:1-4:9),OpColon(4:10-4:11),LowerIdent(4:12-4:13),OpArrow(4:14-4:16),LowerIdent(4:17-4:18),Newline(1:1-1:1),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpBar(5:12-5:13),LowerIdent(5:13-5:14),OpBar(5:14-5:15),LowerIdent(5:16-5:17),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:46),
LowerIdent(8:1-8:8),OpColon(8:9-8:10),LowerIdent(8:11-8:12),Comma(8:12-8:13),LowerIdent(8:14-8:15),OpArrow(8:16-8:18),OpenRound(8:19-8:20),LowerIdent(8:20-8:21),Comma(8:21-8:22),LowerIdent(8:23-8:24),CloseRound(8:24-8:25),Newline(1:1-1:1),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),OpBar(9:11-9:12),LowerIdent(9:12-9:17),Comma(9:17-9:18),LowerIdent(9:19-9:25),OpBar(9:25-9:26),OpenRound(9:27-9:28),LowerIdent(9:28-9:33),Comma(9:33-9:34),LowerIdent(9:35-9:41),CloseRound(9:41-9:42),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:44),
LowerIdent(12:1-12:7),OpColon(12:8-12:9),UpperIdent(12:10-12:13),OpArrow(12:14-12:16),UpperIdent(12:17-12:20),Newline(1:1-1:1),
LowerIdent(13:1-13:7),OpAssign(13:8-13:9),OpBar(13:10-13:11),LowerIdent(13:11-13:12),OpBar(13:12-13:13),LowerIdent(13:14-13:15),OpPlus(13:16-13:17),Int(13:18-13:19),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(15:1-15:6),OpAssign(15:7-15:8),OpBar(15:9-15:10),Underscore(15:10-15:11),OpBar(15:11-15:12),OpenCurly(15:13-15:14),Newline(1:1-1:1),
Newline(16:6-16:41),
LowerIdent(17:5-17:8),OpAssign(17:9-17:10),LowerIdent(17:11-17:19),NoSpaceOpenRound(17:19-17:20),Int(17:20-17:22),CloseRound(17:22-17:23),Newline(1:1-1:1),
LowerIdent(18:5-18:9),OpAssign(18:10-18:11),LowerIdent(18:12-18:20),NoSpaceOpenRound(18:20-18:21),StringStart(18:21-18:22),StringPart(18:22-18:27),StringEnd(18:27-18:28),CloseRound(18:28-18:29),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(20:6-20:28),
LowerIdent(21:5-21:9),OpAssign(21:10-21:11),LowerIdent(21:12-21:19),NoSpaceOpenRound(21:19-21:20),LowerIdent(21:20-21:23),Comma(21:23-21:24),LowerIdent(21:25-21:29),CloseRound(21:29-21:30),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(23:6-23:29),
LowerIdent(24:5-24:11),OpAssign(24:12-24:13),LowerIdent(24:14-24:20),NoSpaceOpenRound(24:20-24:21),Int(24:21-24:22),CloseRound(24:22-24:23),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(26:5-26:11),Newline(1:1-1:1),
CloseCurly(27:1-27:2),EndOfFile(27:2-27:2),
~~~
# PARSE
~~~clojure
(file (1:1-27:2)
	(app (1:1-1:53)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:53)
			"pf"
			(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))
		(packages (1:13-1:53)
			(record_field (1:15-1:53)
				"pf"
				(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))))
	(statements
		(type_anno (4:1-5:9)
			"identity"
			(fn (4:12-4:18)
				(ty_var (4:12-4:13) "a")
				(ty_var (4:17-4:18) "a")))
		(decl (5:1-5:17)
			(ident (5:1-5:9) "identity")
			(lambda (5:12-5:17)
				(args (ident (5:13-5:14) "x"))
				(ident (5:16-5:17) "" "x")))
		(type_anno (8:1-9:8)
			"combine"
			(fn (8:11-8:25)
				(ty_var (8:11-8:12) "a")
				(ty_var (8:14-8:15) "b")
				(tuple (8:19-8:25)
					(ty_var (8:20-8:21) "a")
					(ty_var (8:23-8:24) "b"))))
		(decl (9:1-9:42)
			(ident (9:1-9:8) "combine")
			(lambda (9:11-9:42)
				(args
					(ident (9:12-9:17) "first")
					(ident (9:19-9:25) "second"))
				(tuple (9:27-9:42)
					(ident (9:28-9:33) "" "first")
					(ident (9:35-9:41) "" "second"))))
		(type_anno (12:1-13:7)
			"addOne"
			(fn (12:10-12:20)
				(ty "U64")
				(ty "U64")))
		(decl (13:1-15:6)
			(ident (13:1-13:7) "addOne")
			(lambda (13:10-15:6)
				(args (ident (13:11-13:12) "n"))
				(binop (13:14-15:6)
					"+"
					(ident (13:14-13:15) "" "n")
					(int (13:18-13:19) "1"))))
		(decl (15:1-27:2)
			(ident (15:1-15:6) "main!")
			(lambda (15:9-27:2)
				(args (underscore))
				(block (15:13-27:2)
					(statements
						(decl (17:5-17:23)
							(ident (17:5-17:8) "num")
							(apply (17:11-17:23)
								(ident (17:11-17:19) "" "identity")
								(int (17:20-17:22) "42")))
						(decl (18:5-18:29)
							(ident (18:5-18:9) "text")
							(apply (18:12-18:29)
								(ident (18:12-18:20) "" "identity")
								(string (18:21-18:28) (string_part (18:22-18:27) "hello"))))
						(decl (21:5-21:30)
							(ident (21:5-21:9) "pair")
							(apply (21:12-21:30)
								(ident (21:12-21:19) "" "combine")
								(ident (21:20-21:23) "" "num")
								(ident (21:25-21:29) "" "text")))
						(decl (24:5-24:23)
							(ident (24:5-24:11) "result")
							(apply (24:14-24:23)
								(ident (24:14-24:20) "" "addOne")
								(int (24:21-24:22) "5")))
						(ident (26:5-26:11) "" "result")))))))
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
(can_ir
	(d_let
		(def_pattern
			(p_assign (5:1-5:9)
				(pid 77)
				(ident "identity")))
		(def_expr
			(e_lambda (5:12-5:17)
				(args
					(p_assign (5:13-5:14)
						(pid 78)
						(ident "x")))
				(e_lookup (5:16-5:17) (pid 78))))
		(annotation (5:1-5:9)
			(signature 85)
			(declared_type
				(fn (4:12-4:18)
					(ty_var (4:12-4:13) "a")
					(ty_var (4:17-4:18) "a")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (9:1-9:8)
				(pid 98)
				(ident "combine")))
		(def_expr
			(e_lambda (9:11-9:42)
				(args
					(p_assign (9:12-9:17)
						(pid 99)
						(ident "first"))
					(p_assign (9:19-9:25)
						(pid 100)
						(ident "second")))
				(e_tuple (9:27-9:42)
					(tuple_var "#103")
					(elems
						(e_lookup (9:28-9:33) (pid 99))
						(e_lookup (9:35-9:41) (pid 100))))))
		(annotation (9:1-9:8)
			(signature 111)
			(declared_type
				(fn (8:11-8:25)
					(ty_var (8:11-8:12) "a")
					(ty_var (8:14-8:15) "b")
					(tuple (8:19-8:25)
						(ty_var (8:20-8:21) "a")
						(ty_var (8:23-8:24) "b"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (13:1-13:7)
				(pid 117)
				(ident "addOne")))
		(def_expr
			(e_lambda (13:10-15:6)
				(args
					(p_assign (13:11-13:12)
						(pid 118)
						(ident "n")))
				(e_binop (13:14-15:6)
					"add"
					(e_lookup (13:14-13:15) (pid 118))
					(e_int (13:18-13:19)
						(int_var 121)
						(precision_var 120)
						(literal "1")
						(value "TODO")
						(bound "u8")))))
		(annotation (13:1-13:7)
			(signature 128)
			(declared_type
				(fn (12:10-12:20)
					(ty (12:10-12:13) "U64")
					(ty (12:17-12:20) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (15:1-15:6)
				(pid 131)
				(ident "main!")))
		(def_expr
			(e_lambda (15:9-27:2)
				(args (p_underscore (15:10-15:11) (pid 132)))
				(e_block (15:13-27:2)
					(s_let (17:5-17:23)
						(p_assign (17:5-17:8)
							(pid 133)
							(ident "num"))
						(e_call (17:11-17:23)
							(e_lookup (17:11-17:19) (pid 77))
							(e_int (17:20-17:22)
								(int_var 136)
								(precision_var 135)
								(literal "42")
								(value "TODO")
								(bound "u8"))))
					(s_let (18:5-18:29)
						(p_assign (18:5-18:9)
							(pid 140)
							(ident "text"))
						(e_call (18:12-18:29)
							(e_lookup (18:12-18:20) (pid 77))
							(e_string (18:21-18:28) (e_literal (18:22-18:27) "hello"))))
					(s_let (21:5-21:30)
						(p_assign (21:5-21:9)
							(pid 146)
							(ident "pair"))
						(e_call (21:12-21:30)
							(e_lookup (21:12-21:19) (pid 98))
							(e_lookup (21:20-21:23) (pid 133))
							(e_lookup (21:25-21:29) (pid 140))))
					(s_let (24:5-24:23)
						(p_assign (24:5-24:11)
							(pid 152)
							(ident "result"))
						(e_call (24:14-24:23)
							(e_lookup (24:14-24:20) (pid 117))
							(e_int (24:21-24:22)
								(int_var 155)
								(precision_var 154)
								(literal "5")
								(value "TODO")
								(bound "u8"))))
					(e_lookup (26:5-26:11) (pid 152)))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "identity" 87 (type "*"))
		(def "combine" 113 (type "*"))
		(def "addOne" 130 (type "*"))
		(def "main!" 163 (type "*")))
	(expressions
		(expr (5:12-5:17) 80 (type "*"))
		(expr (9:11-9:42) 105 (type "*"))
		(expr (13:10-15:6) 124 (type "*"))
		(expr (15:9-27:2) 162 (type "*"))))
~~~