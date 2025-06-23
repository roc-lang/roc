# META
~~~ini
description=Two distinct type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

main! = |_| {}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),OpenRound(3:8-3:9),LowerIdent(3:9-3:10),Comma(3:10-3:11),LowerIdent(3:12-3:13),CloseRound(3:13-3:14),OpArrow(3:15-3:17),OpenRound(3:18-3:19),LowerIdent(3:19-3:20),Comma(3:20-3:21),LowerIdent(3:22-3:23),CloseRound(3:23-3:24),Newline(1:1-1:1),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),NoSpaceOpenRound(4:9-4:10),LowerIdent(4:10-4:11),Comma(4:11-4:12),LowerIdent(4:13-4:14),CloseRound(4:14-4:15),OpBar(4:15-4:16),OpenRound(4:17-4:18),LowerIdent(4:18-4:19),Comma(4:19-4:20),LowerIdent(4:21-4:22),CloseRound(4:22-4:23),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file (1:1-6:15)
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
		(type_anno (3:1-4:5)
			"swap"
			(fn (3:8-3:24)
				(tuple (3:8-3:14)
					(ty_var (3:9-3:10) "a")
					(ty_var (3:12-3:13) "b"))
				(tuple (3:18-3:24)
					(ty_var (3:19-3:20) "b")
					(ty_var (3:22-3:23) "a"))))
		(decl (4:1-4:23)
			(ident (4:1-4:5) "swap")
			(lambda (4:8-4:23)
				(args
					(tuple (4:9-4:15)
						(ident (4:10-4:11) "x")
						(ident (4:13-4:14) "y")))
				(tuple (4:17-4:23)
					(ident (4:18-4:19) "" "y")
					(ident (4:21-4:22) "" "x"))))
		(decl (6:1-6:15)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:15)
				(args (underscore))
				(record (6:13-6:15))))))
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
			(p_assign (4:1-4:5)
				(pid 83)
				(ident "swap")))
		(def_expr
			(e_lambda (4:8-4:23)
				(args
					(p_tuple (4:9-4:15)
						(pid 87)
						(tuple_var "#86")
						(patterns
							(p_assign (4:10-4:11)
								(pid 84)
								(ident "x"))
							(p_assign (4:13-4:14)
								(pid 85)
								(ident "y")))))
				(e_tuple (4:17-4:23)
					(tuple_var "#90")
					(elems
						(e_lookup (4:18-4:19) (pid 85))
						(e_lookup (4:21-4:22) (pid 84))))))
		(annotation (4:1-4:5)
			(signature 96)
			(declared_type
				(fn (3:8-3:24)
					(tuple (3:8-3:14)
						(ty_var (3:9-3:10) "a")
						(ty_var (3:12-3:13) "b"))
					(tuple (3:18-3:24)
						(ty_var (3:19-3:20) "b")
						(ty_var (3:22-3:23) "a"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 99)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:15)
				(args (p_underscore (6:10-6:11) (pid 100)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "swap" 98 (type "*"))
		(def "main!" 104 (type "*")))
	(expressions
		(expr (4:8-4:23) 92 (type "*"))
		(expr (6:9-6:15) 103 (type "*"))))
~~~