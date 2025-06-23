# META
~~~ini
description=Two decls
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

a = 5
b = a + 1
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),LowerIdent(4:5-4:6),OpPlus(4:7-4:8),Int(4:9-4:10),EndOfFile(4:10-4:10),
~~~
# PARSE
~~~clojure
(file (1:1-4:10)
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
		(decl (3:1-3:6)
			(ident (3:1-3:2) "a")
			(int (3:5-3:6) "5"))
		(decl (4:1-4:10)
			(ident (4:1-4:2) "b")
			(binop (4:5-4:10)
				"+"
				(ident (4:5-4:6) "" "a")
				(int (4:9-4:10) "1")))))
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
			(p_assign (3:1-3:2)
				(pid 72)
				(ident "a")))
		(def_expr
			(e_int (3:5-3:6)
				(int_var 74)
				(precision_var 73)
				(literal "5")
				(value "TODO")
				(bound "u8"))))
	(d_let
		(def_pattern
			(p_assign (4:1-4:2)
				(pid 77)
				(ident "b")))
		(def_expr
			(e_binop (4:5-4:10)
				"add"
				(e_lookup_local (4:5-4:6) (pid 72))
				(e_int (4:9-4:10)
					(int_var 80)
					(precision_var 79)
					(literal "1")
					(value "TODO")
					(bound "u8"))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "a" 76 (type "Num(Int(*))"))
		(def "b" 83 (type "*")))
	(expressions
		(expr (3:5-3:6) 75 (type "Num(Int(*))"))
		(expr (4:5-4:10) 82 (type "*"))))
~~~