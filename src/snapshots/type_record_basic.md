# META
~~~ini
description=Basic record type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({name: "luke", age:21})
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenCurly(3:11-3:12),LowerIdent(3:13-3:17),OpColon(3:17-3:18),UpperIdent(3:19-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:27),OpColon(3:27-3:28),UpperIdent(3:29-3:32),CloseCurly(3:33-3:34),OpArrow(3:35-3:37),UpperIdent(3:38-3:41),Newline(1:1-1:1),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),NamedUnderscore(4:12-4:19),OpBar(4:19-4:20),StringStart(4:21-4:22),StringPart(4:22-4:27),StringEnd(4:27-4:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:20),NoSpaceOpenRound(6:20-6:21),OpenCurly(6:21-6:22),LowerIdent(6:22-6:26),OpColon(6:26-6:27),StringStart(6:28-6:29),StringPart(6:29-6:33),StringEnd(6:33-6:34),Comma(6:34-6:35),LowerIdent(6:36-6:39),OpColon(6:39-6:40),Int(6:40-6:42),CloseCurly(6:42-6:43),CloseRound(6:43-6:44),EndOfFile(6:44-6:44),
~~~
# PARSE
~~~clojure
(file (1:1-6:44)
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
		(type_anno (3:1-4:8)
			"getName"
			(fn (3:11-3:41)
				(record (3:11-3:34)
					(anno_record_field (3:13-3:23) "name" (ty "Str"))
					(anno_record_field (3:24-3:34) "age" (ty "U64")))
				(ty "Str")))
		(decl (4:1-4:28)
			(ident (4:1-4:8) "getName")
			(lambda (4:11-4:28)
				(args (ident (4:12-4:19) "_person"))
				(string (4:21-4:28) (string_part (4:22-4:27) "hello"))))
		(decl (6:1-6:44)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:44)
				(args (underscore))
				(apply (6:13-6:44)
					(ident (6:13-6:20) "" "getName")
					(record (6:21-6:43)
						(field
							"name"
							(string (6:28-6:34) (string_part (6:29-6:33) "luke")))
						(field "age" (int (6:40-6:42) "21"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name : Str, age : U64 } -> Str
getName = |_person| "hello"

main! = |_| getName({ name: "luke", age: 21 })
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:8)
				(pid 79)
				(ident "getName")))
		(def_expr
			(e_lambda (4:11-4:28)
				(args
					(p_assign (4:12-4:19)
						(pid 80)
						(ident "_person")))
				(e_string (4:21-4:28) (e_literal (4:22-4:27) "hello"))))
		(annotation (4:1-4:8)
			(signature 87)
			(declared_type
				(fn (3:11-3:41)
					(record (3:11-3:34)
						(record_field "name" (ty (3:19-3:22) "Str"))
						(record_field "age" (ty (3:29-3:32) "U64")))
					(ty (3:38-3:41) "Str")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 90)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:44)
				(args (p_underscore (6:10-6:11) (pid 91)))
				(e_call (6:13-6:44)
					(e_lookup_local (6:13-6:20) (pid 79))
					(e_runtime_error (1:1-1:1) "not_implemented"))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "getName" 89 (type "*"))
		(def "main!" 97 (type "*")))
	(expressions
		(expr (4:11-4:28) 83 (type "*"))
		(expr (6:9-6:44) 96 (type "*"))))
~~~