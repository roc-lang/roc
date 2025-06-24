# META
~~~ini
description=Simple type alias usage in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

UserId : U64

getUser : UserId -> Str
getUser = |id| if (id > 10) "big" else "small"

main! = |_| getUser(100)
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize if_then_else expression

**UNUSED VARIABLE**
Variable ``id`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_id` to suppress this warning.
The unused variable is declared here:
**type_alias_simple.md:6:12:6:14:**
```roc
getUser = |id| if (id > 10) "big" else "small"
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:17),OpArrow(5:18-5:20),UpperIdent(5:21-5:24),Newline(1:1-1:1),
LowerIdent(6:1-6:8),OpAssign(6:9-6:10),OpBar(6:11-6:12),LowerIdent(6:12-6:14),OpBar(6:14-6:15),KwIf(6:16-6:18),OpenRound(6:19-6:20),LowerIdent(6:20-6:22),OpGreaterThan(6:23-6:24),Int(6:25-6:27),CloseRound(6:27-6:28),StringStart(6:29-6:30),StringPart(6:30-6:33),StringEnd(6:33-6:34),KwElse(6:35-6:39),StringStart(6:40-6:41),StringPart(6:41-6:46),StringEnd(6:46-6:47),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),LowerIdent(8:13-8:20),NoSpaceOpenRound(8:20-8:21),Int(8:21-8:24),CloseRound(8:24-8:25),EndOfFile(8:25-8:25),
~~~
# PARSE
~~~clojure
(file (1:1-8:25)
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
		(type_decl (3:1-5:8)
			(header (3:1-3:7) "UserId" (args))
			(ty "U64"))
		(type_anno (5:1-6:8)
			"getUser"
			(fn (5:11-5:24)
				(ty "UserId")
				(ty "Str")))
		(decl (1:1-1:1)
			(ident (6:1-6:8) "getUser")
			(lambda (1:1-1:1)
				(args (ident (6:12-6:14) "id"))
				(if_then_else (1:1-1:1)
					(tuple (6:19-6:28)
						(binop (6:20-6:28)
							">"
							(ident (6:20-6:22) "" "id")
							(int (6:25-6:27) "10")))
					(string (6:29-6:34) (string_part (6:30-6:33) "big"))
					(string (6:40-6:47) (string_part (6:41-6:46) "small")))))
		(decl (8:1-8:25)
			(ident (8:1-8:6) "main!")
			(lambda (8:9-8:25)
				(args (underscore))
				(apply (8:13-8:25)
					(ident (8:13-8:20) "" "getUser")
					(int (8:21-8:24) "100"))))))
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
			(p_assign (6:1-6:8)
				(pid 78)
				(ident "getUser")))
		(def_expr
			(e_lambda (1:1-1:1)
				(args
					(p_assign (6:12-6:14)
						(pid 79)
						(ident "id")))
				(e_runtime_error (1:1-1:1) "not_implemented")))
		(annotation (6:1-6:8)
			(signature 87)
			(declared_type
				(fn (5:11-5:24)
					(ty (5:11-5:17) "UserId")
					(ty (5:21-5:24) "Str")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (8:1-8:6)
				(pid 90)
				(ident "main!")))
		(def_expr
			(e_lambda (8:9-8:25)
				(args (p_underscore (8:10-8:11) (pid 91)))
				(e_call (8:13-8:25)
					(e_lookup_local (8:13-8:20) (pid 78))
					(e_int (8:21-8:24)
						(int_var 94)
						(precision_var 93)
						(literal "100")
						(value "TODO")
						(bound "u8"))))))
	(s_type_decl (3:1-5:8)
		(type_header (3:1-3:7) "UserId")
		(ty (3:10-3:13) "U64")))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "getUser" 89 (type "*"))
		(def "main!" 98 (type "*")))
	(expressions
		(expr (1:1-1:1) 82 (type "*"))
		(expr (8:9-8:25) 97 (type "*"))))
~~~