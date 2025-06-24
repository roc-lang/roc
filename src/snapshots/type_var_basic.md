# META
~~~ini
description=Basic type variable introduction in type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'a' introduced in annotation and used in body
identity : a -> a
identity = |a| a

main! = |_| {}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:62),
LowerIdent(4:1-4:9),OpColon(4:10-4:11),LowerIdent(4:12-4:13),OpArrow(4:14-4:16),LowerIdent(4:17-4:18),Newline(1:1-1:1),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpBar(5:12-5:13),LowerIdent(5:13-5:14),OpBar(5:14-5:15),LowerIdent(5:16-5:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),OpBar(7:9-7:10),Underscore(7:10-7:11),OpBar(7:11-7:12),OpenCurly(7:13-7:14),CloseCurly(7:14-7:15),EndOfFile(7:15-7:15),
~~~
# PARSE
~~~clojure
(file (1:1-7:15)
	(app (1:1-1:57)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:57)
			"pf"
			(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))
		(packages (1:13-1:57)
			(record_field (1:15-1:57)
				"pf"
				(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))))
	(statements
		(type_anno (4:1-5:9)
			"identity"
			(fn (4:12-4:18)
				(ty_var (4:12-4:13) "a")
				(ty_var (4:17-4:18) "a")))
		(decl (5:1-5:17)
			(ident (5:1-5:9) "identity")
			(lambda (5:12-5:17)
				(args (ident (5:13-5:14) "a"))
				(ident (5:16-5:17) "" "a")))
		(decl (7:1-7:15)
			(ident (7:1-7:6) "main!")
			(lambda (7:9-7:15)
				(args (underscore))
				(record (7:13-7:15))))))
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
			(p_assign (5:1-5:9)
				(pid 77)
				(ident "identity")))
		(def_expr
			(e_lambda (5:12-5:17)
				(args
					(p_assign (5:13-5:14)
						(pid 78)
						(ident "a")))
				(e_lookup_local (5:16-5:17) (pid 78))))
		(annotation (5:1-5:9)
			(signature 85)
			(declared_type
				(fn (4:12-4:18)
					(ty_var (4:12-4:13) "a")
					(ty_var (4:17-4:18) "a")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (7:1-7:6)
				(pid 88)
				(ident "main!")))
		(def_expr
			(e_lambda (7:9-7:15)
				(args (p_underscore (7:10-7:11) (pid 89)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "identity" 87 (type "*"))
		(def "main!" 93 (type "*")))
	(expressions
		(expr (5:12-5:17) 80 (type "*"))
		(expr (7:9-7:15) 92 (type "*"))))
~~~