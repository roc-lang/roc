# META
~~~ini
description=Record with type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field: a, other: b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# PROBLEMS
**UNDECLARED TYPE VARIABLE**
The type variable ``b`` is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**type_record_with_vars.md:3:31:3:32:**
```roc
getField : { field: a, other: b } -> a
```


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),OpenCurly(3:12-3:13),LowerIdent(3:14-3:19),OpColon(3:19-3:20),LowerIdent(3:21-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:29),OpColon(3:29-3:30),LowerIdent(3:31-3:32),CloseCurly(3:33-3:34),OpArrow(3:35-3:37),LowerIdent(3:38-3:39),Newline(1:1-1:1),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:19),OpBar(4:19-4:20),LowerIdent(4:21-4:27),NoSpaceDotLowerIdent(4:27-4:33),Newline(1:1-1:1),
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
		(type_anno (3:1-4:9)
			"getField"
			(fn (3:12-3:39)
				(record (3:12-3:34)
					(anno_record_field (3:14-3:23)
						"field"
						(ty_var (3:21-3:22) "a"))
					(anno_record_field (3:24-3:34)
						"other"
						(ty_var (3:31-3:32) "b")))
				(ty_var (3:38-3:39) "a")))
		(decl (4:1-6:6)
			(ident (4:1-4:9) "getField")
			(lambda (4:12-6:6)
				(args (ident (4:13-4:19) "record"))
				(field_access (4:21-6:6)
					(binop (4:21-6:6)
						"app"
						(ident (4:21-4:27) "" "record")
						(ident (4:27-4:33) "" ".field")))))
		(decl (6:1-6:15)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:15)
				(args (underscore))
				(record (6:13-6:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field : a, other : b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:9)
				(pid 81)
				(ident "getField")))
		(def_expr
			(e_lambda (4:12-6:6)
				(args
					(p_assign (4:13-4:19)
						(pid 82)
						(ident "record")))
				(e_dot_access (4:21-6:6)
					(e_lookup_local (4:21-4:27) (pid 82))
					"field")))
		(annotation (4:1-4:9)
			(signature 90)
			(declared_type
				(fn (3:12-3:39)
					(record (3:12-3:34)
						(record_field "field" (ty_var (3:21-3:22) "a"))
						(record_field "other" (ty_var (3:31-3:32) "b")))
					(ty_var (3:38-3:39) "a")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 93)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:15)
				(args (p_underscore (6:10-6:11) (pid 94)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "getField" 92 (type "*"))
		(def "main!" 98 (type "*")))
	(expressions
		(expr (4:12-6:6) 85 (type "*"))
		(expr (6:9-6:15) 97 (type "*"))))
~~~