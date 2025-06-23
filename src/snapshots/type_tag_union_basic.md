# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |maybe| "result"

main! = |_| {}
~~~
# PROBLEMS
**UNDECLARED TYPE**
The type ``None`` is not declared in this scope.

This type is referenced here:
**type_tag_union_basic.md:3:23:3:27:**
```roc
process : [Some(Str), None] -> Str
```


**UNUSED VARIABLE**
Variable ``maybe`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_maybe` to suppress this warning.
The unused variable is declared here:
**type_tag_union_basic.md:4:12:4:17:**
```roc
process = |maybe| "result"
```


**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenSquare(3:11-3:12),UpperIdent(3:12-3:16),NoSpaceOpenRound(3:16-3:17),UpperIdent(3:17-3:20),CloseRound(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:35),Newline(1:1-1:1),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:17),OpBar(4:17-4:18),StringStart(4:19-4:20),StringPart(4:20-4:26),StringEnd(4:26-4:27),Newline(1:1-1:1),
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
		(type_anno (3:1-4:8)
			"process"
			(fn (3:11-3:35)
				(tag_union (3:11-3:28)
					(tags
						(apply (3:12-3:21)
							(ty "Some")
							(ty "Str"))
						(ty "None")))
				(ty "Str")))
		(decl (4:1-4:27)
			(ident (4:1-4:8) "process")
			(lambda (4:11-4:27)
				(args (ident (4:12-4:17) "maybe"))
				(string (4:19-4:27) (string_part (4:20-4:26) "result"))))
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
			(p_assign (4:1-4:8)
				(pid 79)
				(ident "process")))
		(def_expr
			(e_lambda (4:11-4:27)
				(args
					(p_assign (4:12-4:17)
						(pid 80)
						(ident "maybe")))
				(e_string (4:19-4:27) (e_literal (4:20-4:26) "result"))))
		(annotation (4:1-4:8)
			(signature 88)
			(declared_type
				(fn (3:11-3:35)
					(tag_union (3:11-3:28)
						(apply (3:12-3:21)
							"Some"
							(ty (3:17-3:20) "Str"))
						(ty (3:23-3:27) "None"))
					(ty (3:32-3:35) "Str")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 91)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:15)
				(args (p_underscore (6:10-6:11) (pid 92)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "process" 90 (type "*"))
		(def "main!" 96 (type "*")))
	(expressions
		(expr (4:11-4:27) 83 (type "*"))
		(expr (6:9-6:15) 95 (type "*"))))
~~~