# META
~~~ini
description=Simple function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

apply : (a -> b), a -> b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **, a** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.
Here is the problematic code:
**type_function_simple.md:3:17:3:20:**
```roc
apply : (a -> b), a -> b
```


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),OpenRound(3:9-3:10),LowerIdent(3:10-3:11),OpArrow(3:12-3:14),LowerIdent(3:15-3:16),CloseRound(3:16-3:17),Comma(3:17-3:18),LowerIdent(3:19-3:20),OpArrow(3:21-3:23),LowerIdent(3:24-3:25),Newline(1:1-1:1),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:12),Comma(4:12-4:13),LowerIdent(4:14-4:15),OpBar(4:15-4:16),LowerIdent(4:17-4:19),NoSpaceOpenRound(4:19-4:20),LowerIdent(4:20-4:21),CloseRound(4:21-4:22),Newline(1:1-1:1),
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
		(type_anno (3:1-3:18)
			"apply"
			(fn (3:10-3:16)
				(ty_var (3:10-3:11) "a")
				(ty_var (3:15-3:16) "b")))
		(malformed_expr (3:17-3:20) "expr_unexpected_token")
		(local_dispatch (3:19-4:6)
			(ident (3:19-3:20) "" "a")
			(ident (3:24-3:25) "" "b"))
		(decl (4:1-4:22)
			(ident (4:1-4:6) "apply")
			(lambda (4:9-4:22)
				(args
					(ident (4:10-4:12) "fn")
					(ident (4:14-4:15) "x"))
				(apply (4:17-4:22)
					(ident (4:17-4:19) "" "fn")
					(ident (4:20-4:21) "" "x"))))
		(decl (6:1-6:15)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:15)
				(args (underscore))
				(record (6:13-6:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

apply : (a -> b)a->b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:6)
				(pid 80)
				(ident "apply")))
		(def_expr
			(e_lambda (4:9-4:22)
				(args
					(p_assign (4:10-4:12)
						(pid 81)
						(ident "fn"))
					(p_assign (4:14-4:15)
						(pid 82)
						(ident "x")))
				(e_call (4:17-4:22)
					(e_lookup_local (4:17-4:19) (pid 81))
					(e_lookup_local (4:20-4:21) (pid 82)))))
		(annotation (4:1-4:6)
			(signature 92)
			(declared_type
				(parens (3:9-3:17)
					(fn (3:10-3:16)
						(ty_var (3:10-3:11) "a")
						(ty_var (3:15-3:16) "b")
						"false")))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 95)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:15)
				(args (p_underscore (6:10-6:11) (pid 96)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "apply" 94 (type "*"))
		(def "main!" 100 (type "*")))
	(expressions
		(expr (4:9-4:22) 86 (type "*"))
		(expr (6:9-6:15) 99 (type "*"))))
~~~