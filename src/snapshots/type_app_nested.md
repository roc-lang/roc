# META
~~~ini
description=Nested type applications in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processNested : List(Result(Str, Err)) -> List(Str)
processNested = |_list| ["one","two"]

main! = |_| processNested([])
~~~
# PROBLEMS
**UNDECLARED TYPE**
The type ``Err`` is not declared in this scope.

This type is referenced here:
**type_app_nested.md:3:34:3:37:**
```roc
processNested : List(Result(Str, Err)) -> List(Str)
```


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:14),OpColon(3:15-3:16),UpperIdent(3:17-3:21),NoSpaceOpenRound(3:21-3:22),UpperIdent(3:22-3:28),NoSpaceOpenRound(3:28-3:29),UpperIdent(3:29-3:32),Comma(3:32-3:33),UpperIdent(3:34-3:37),CloseRound(3:37-3:38),CloseRound(3:38-3:39),OpArrow(3:40-3:42),UpperIdent(3:43-3:47),NoSpaceOpenRound(3:47-3:48),UpperIdent(3:48-3:51),CloseRound(3:51-3:52),Newline(1:1-1:1),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpBar(4:17-4:18),NamedUnderscore(4:18-4:23),OpBar(4:23-4:24),OpenSquare(4:25-4:26),StringStart(4:26-4:27),StringPart(4:27-4:30),StringEnd(4:30-4:31),Comma(4:31-4:32),StringStart(4:32-4:33),StringPart(4:33-4:36),StringEnd(4:36-4:37),CloseSquare(4:37-4:38),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:26),NoSpaceOpenRound(6:26-6:27),OpenSquare(6:27-6:28),CloseSquare(6:28-6:29),CloseRound(6:29-6:30),EndOfFile(6:30-6:30),
~~~
# PARSE
~~~clojure
(file (1:1-6:30)
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
		(type_anno (3:1-4:14)
			"processNested"
			(fn (3:17-3:52)
				(apply (3:17-3:39)
					(ty "List")
					(apply (3:22-3:38)
						(ty "Result")
						(ty "Str")
						(ty "Err")))
				(apply (3:43-3:52)
					(ty "List")
					(ty "Str"))))
		(decl (4:1-4:38)
			(ident (4:1-4:14) "processNested")
			(lambda (4:17-4:38)
				(args (ident (4:18-4:23) "_list"))
				(list (4:25-4:38)
					(string (4:26-4:31) (string_part (4:27-4:30) "one"))
					(string (4:32-4:37) (string_part (4:33-4:36) "two")))))
		(decl (6:1-6:30)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:30)
				(args (underscore))
				(apply (6:13-6:30)
					(ident (6:13-6:26) "" "processNested")
					(list (6:27-6:29)))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processNested : List(Result(Str, Err)) -> List(Str)
processNested = |_list| ["one", "two"]

main! = |_| processNested([])
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:14)
				(pid 80)
				(ident "processNested")))
		(def_expr
			(e_lambda (4:17-4:38)
				(args
					(p_assign (4:18-4:23)
						(pid 81)
						(ident "_list")))
				(e_list (4:25-4:38)
					(elem_var 86)
					(elems
						(e_string (4:26-4:31) (e_literal (4:27-4:30) "one"))
						(e_string (4:32-4:37) (e_literal (4:33-4:36) "two"))))))
		(annotation (4:1-4:14)
			(signature 92)
			(declared_type
				(fn (3:17-3:52)
					(apply (3:17-3:39)
						"List"
						(apply (3:22-3:38)
							"Result"
							(ty (3:29-3:32) "Str")
							(ty (3:34-3:37) "Err")))
					(apply (3:43-3:52)
						"List"
						(ty (3:48-3:51) "Str"))
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 95)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:30)
				(args (p_underscore (6:10-6:11) (pid 96)))
				(e_call (6:13-6:30)
					(e_lookup_local (6:13-6:26) (pid 80))
					(e_list (6:27-6:29) (elem_var 98) (elems)))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "processNested" 94 (type "*"))
		(def "main!" 102 (type "*")))
	(expressions
		(expr (4:17-4:38) 88 (type "*"))
		(expr (6:9-6:30) 101 (type "*"))))
~~~