# META
~~~ini
description=Parameterized type alias with type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

Pair(a, b) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |(x, y)| (y, x)

main! = |_| swapPair(1, 2)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),CloseRound(3:10-3:11),OpColon(3:12-3:13),OpenRound(3:14-3:15),LowerIdent(3:15-3:16),Comma(3:16-3:17),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:9),OpColon(5:10-5:11),UpperIdent(5:12-5:16),NoSpaceOpenRound(5:16-5:17),LowerIdent(5:17-5:18),Comma(5:18-5:19),LowerIdent(5:20-5:21),CloseRound(5:21-5:22),OpArrow(5:23-5:25),UpperIdent(5:26-5:30),NoSpaceOpenRound(5:30-5:31),LowerIdent(5:31-5:32),Comma(5:32-5:33),LowerIdent(5:34-5:35),CloseRound(5:35-5:36),Newline(1:1-1:1),
LowerIdent(6:1-6:9),OpAssign(6:10-6:11),OpBar(6:12-6:13),NoSpaceOpenRound(6:13-6:14),LowerIdent(6:14-6:15),Comma(6:15-6:16),LowerIdent(6:17-6:18),CloseRound(6:18-6:19),OpBar(6:19-6:20),OpenRound(6:21-6:22),LowerIdent(6:22-6:23),Comma(6:23-6:24),LowerIdent(6:25-6:26),CloseRound(6:26-6:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),LowerIdent(8:13-8:21),NoSpaceOpenRound(8:21-8:22),Int(8:22-8:23),Comma(8:23-8:24),Int(8:25-8:26),CloseRound(8:26-8:27),EndOfFile(8:27-8:27),
~~~
# PARSE
~~~clojure
(file (1:1-8:27)
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
		(type_decl (3:1-5:9)
			(header (3:1-3:11)
				"Pair"
				(args
					(ty_var (3:6-3:7) "a")
					(ty_var (3:9-3:10) "b")))
			(tuple (3:14-3:20)
				(ty_var (3:15-3:16) "a")
				(ty_var (3:18-3:19) "b")))
		(type_anno (5:1-6:9)
			"swapPair"
			(fn (5:12-5:36)
				(apply (5:12-5:22)
					(ty "Pair")
					(ty_var (5:17-5:18) "a")
					(ty_var (5:20-5:21) "b"))
				(apply (5:26-5:36)
					(ty "Pair")
					(ty_var (5:31-5:32) "b")
					(ty_var (5:34-5:35) "a"))))
		(decl (6:1-6:27)
			(ident (6:1-6:9) "swapPair")
			(lambda (6:12-6:27)
				(args
					(tuple (6:13-6:19)
						(ident (6:14-6:15) "x")
						(ident (6:17-6:18) "y")))
				(tuple (6:21-6:27)
					(ident (6:22-6:23) "" "y")
					(ident (6:25-6:26) "" "x"))))
		(decl (8:1-8:27)
			(ident (8:1-8:6) "main!")
			(lambda (8:9-8:27)
				(args (underscore))
				(apply (8:13-8:27)
					(ident (8:13-8:21) "" "swapPair")
					(int (8:22-8:23) "1")
					(int (8:25-8:26) "2"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (6:1-6:9)
				(pid 90)
				(ident "swapPair")))
		(def_expr
			(e_lambda (6:12-6:27)
				(args
					(p_tuple (6:13-6:19)
						(pid 94)
						(tuple_var "#93")
						(patterns
							(p_assign (6:14-6:15)
								(pid 91)
								(ident "x"))
							(p_assign (6:17-6:18)
								(pid 92)
								(ident "y")))))
				(e_tuple (6:21-6:27)
					(tuple_var "#97")
					(elems
						(e_lookup (6:22-6:23) (pid 92))
						(e_lookup (6:25-6:26) (pid 91))))))
		(annotation (6:1-6:9)
			(signature 103)
			(declared_type
				(fn (5:12-5:36)
					(apply (5:12-5:22)
						"Pair"
						(ty_var (5:17-5:18) "a")
						(ty_var (5:20-5:21) "b"))
					(apply (5:26-5:36)
						"Pair"
						(ty_var (5:31-5:32) "b")
						(ty_var (5:34-5:35) "a"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (8:1-8:6)
				(pid 106)
				(ident "main!")))
		(def_expr
			(e_lambda (8:9-8:27)
				(args (p_underscore (8:10-8:11) (pid 107)))
				(e_call (8:13-8:27)
					(e_lookup (8:13-8:21) (pid 90))
					(e_int (8:22-8:23)
						(int_var 110)
						(precision_var 109)
						(literal "1")
						(value "TODO")
						(bound "u8"))
					(e_int (8:25-8:26)
						(int_var 113)
						(precision_var 112)
						(literal "2")
						(value "TODO")
						(bound "u8"))))))
	(s_type_decl (3:1-5:9)
		(type_header (3:1-3:11)
			"Pair"
			(args
				(ty_var (3:6-3:7) "a")
				(ty_var (3:9-3:10) "b")))
		(tuple (3:14-3:20)
			(ty_var (3:15-3:16) "a")
			(ty_var (3:18-3:19) "b"))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "swapPair" 105 (type "*"))
		(def "main!" 117 (type "*")))
	(expressions
		(expr (6:12-6:27) 99 (type "*"))
		(expr (8:9-8:27) 116 (type "*"))))
~~~