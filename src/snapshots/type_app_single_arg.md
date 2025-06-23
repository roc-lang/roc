# META
~~~ini
description=Single type argument application in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one","two"])
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),UpperIdent(3:15-3:19),NoSpaceOpenRound(3:19-3:20),UpperIdent(3:20-3:23),CloseRound(3:23-3:24),OpArrow(3:25-3:27),UpperIdent(3:28-3:31),Newline(1:1-1:1),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpBar(4:15-4:16),LowerIdent(4:16-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:26),NoSpaceDotLowerIdent(4:26-4:30),NoSpaceOpenRound(4:30-4:31),CloseRound(4:31-4:32),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:24),NoSpaceOpenRound(6:24-6:25),OpenSquare(6:25-6:26),StringStart(6:26-6:27),StringPart(6:27-6:30),StringEnd(6:30-6:31),Comma(6:31-6:32),StringStart(6:32-6:33),StringPart(6:33-6:36),StringEnd(6:36-6:37),CloseSquare(6:37-6:38),CloseRound(6:38-6:39),EndOfFile(6:39-6:39),
~~~
# PARSE
~~~clojure
(file (1:1-6:39)
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
		(type_anno (3:1-4:12)
			"processList"
			(fn (3:15-3:31)
				(apply (3:15-3:24)
					(ty "List")
					(ty "Str"))
				(ty "U64")))
		(decl (4:1-6:6)
			(ident (4:1-4:12) "processList")
			(lambda (4:15-6:6)
				(args (ident (4:16-4:20) "list"))
				(field_access (4:22-6:6)
					(binop (4:22-6:6)
						"app"
						(ident (4:22-4:26) "" "list")
						(apply (4:26-4:32)
							(ident (4:26-4:30) "" ".len"))))))
		(decl (6:1-6:39)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:39)
				(args (underscore))
				(apply (6:13-6:39)
					(ident (6:13-6:24) "" "processList")
					(list (6:25-6:38)
						(string (6:26-6:31) (string_part (6:27-6:30) "one"))
						(string (6:32-6:37) (string_part (6:33-6:36) "two"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one", "two"])
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:12)
				(pid 76)
				(ident "processList")))
		(def_expr
			(e_lambda (4:15-6:6)
				(args
					(p_assign (4:16-4:20)
						(pid 77)
						(ident "list")))
				(e_dot_access (4:22-6:6)
					(e_lookup (4:22-4:26) (pid 77))
					"len")))
		(annotation (4:1-4:12)
			(signature 84)
			(declared_type
				(fn (3:15-3:31)
					(apply (3:15-3:24)
						"List"
						(ty (3:20-3:23) "Str"))
					(ty (3:28-3:31) "U64")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 87)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:39)
				(args (p_underscore (6:10-6:11) (pid 88)))
				(e_call (6:13-6:39)
					(e_lookup (6:13-6:24) (pid 76))
					(e_list (6:25-6:38)
						(elem_var 94)
						(elems
							(e_string (6:26-6:31) (e_literal (6:27-6:30) "one"))
							(e_string (6:32-6:37) (e_literal (6:33-6:36) "two")))))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "processList" 86 (type "*"))
		(def "main!" 98 (type "*")))
	(expressions
		(expr (4:15-6:6) 80 (type "*"))
		(expr (6:9-6:39) 97 (type "*"))))
~~~