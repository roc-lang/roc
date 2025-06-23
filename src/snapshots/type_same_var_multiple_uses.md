# META
~~~ini
description=Multiple uses of same type variable in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

pair : a -> (a, a)
pair = |x| (x, x)

main! = |_| {}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),LowerIdent(3:8-3:9),OpArrow(3:10-3:12),OpenRound(3:13-3:14),LowerIdent(3:14-3:15),Comma(3:15-3:16),LowerIdent(3:17-3:18),CloseRound(3:18-3:19),Newline(1:1-1:1),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),LowerIdent(4:9-4:10),OpBar(4:10-4:11),OpenRound(4:12-4:13),LowerIdent(4:13-4:14),Comma(4:14-4:15),LowerIdent(4:16-4:17),CloseRound(4:17-4:18),Newline(1:1-1:1),
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
			"pair"
			(fn (3:8-3:19)
				(ty_var (3:8-3:9) "a")
				(tuple (3:13-3:19)
					(ty_var (3:14-3:15) "a")
					(ty_var (3:17-3:18) "a"))))
		(decl (4:1-4:18)
			(ident (4:1-4:5) "pair")
			(lambda (4:8-4:18)
				(args (ident (4:9-4:10) "x"))
				(tuple (4:12-4:18)
					(ident (4:13-4:14) "" "x")
					(ident (4:16-4:17) "" "x"))))
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
				(pid 80)
				(ident "pair")))
		(def_expr
			(e_lambda (4:8-4:18)
				(args
					(p_assign (4:9-4:10)
						(pid 81)
						(ident "x")))
				(e_tuple (4:12-4:18)
					(tuple_var "#84")
					(elems
						(e_lookup (4:13-4:14) (pid 81))
						(e_lookup (4:16-4:17) (pid 81))))))
		(annotation (4:1-4:5)
			(signature 91)
			(declared_type
				(fn (3:8-3:19)
					(ty_var (3:8-3:9) "a")
					(tuple (3:13-3:19)
						(ty_var (3:14-3:15) "a")
						(ty_var (3:17-3:18) "a"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 94)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:15)
				(args (p_underscore (6:10-6:11) (pid 95)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "pair" 93 (type "*"))
		(def "main!" 99 (type "*")))
	(expressions
		(expr (4:8-4:18) 86 (type "*"))
		(expr (6:9-6:15) 98 (type "*"))))
~~~