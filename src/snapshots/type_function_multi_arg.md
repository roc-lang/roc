# META
~~~ini
description=Multi-argument function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

curry : (a, b -> c) -> (a -> b -> c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-> (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_multi_arg.md:3:21:3:25:**
```roc
curry : (a, b -> c) -> (a -> b -> c)
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
LowerIdent(3:1-3:6),OpColon(3:7-3:8),OpenRound(3:9-3:10),LowerIdent(3:10-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:14),OpArrow(3:15-3:17),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),OpArrow(3:21-3:23),OpenRound(3:24-3:25),LowerIdent(3:25-3:26),OpArrow(3:27-3:29),LowerIdent(3:30-3:31),OpArrow(3:32-3:34),LowerIdent(3:35-3:36),CloseRound(3:36-3:37),Newline(1:1-1:1),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:12),OpBar(4:12-4:13),OpBar(4:14-4:15),LowerIdent(4:15-4:16),OpBar(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:24),NoSpaceOpenRound(4:24-4:25),LowerIdent(4:25-4:26),Comma(4:26-4:27),LowerIdent(4:28-4:29),CloseRound(4:29-4:30),Newline(1:1-1:1),
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
		(type_anno (3:1-3:23)
			"curry"
			(fn (3:10-3:19)
				(ty_var (3:10-3:11) "a")
				(ty_var (3:13-3:14) "b")
				(ty_var (3:18-3:19) "c")))
		(malformed_expr (3:21-3:25) "expr_unexpected_token")
		(tuple (3:24-3:37)
			(local_dispatch (3:25-3:37)
				(local_dispatch (3:25-3:34)
					(ident (3:25-3:26) "" "a")
					(ident (3:30-3:31) "" "b"))
				(ident (3:35-3:36) "" "c")))
		(decl (4:1-4:30)
			(ident (4:1-4:6) "curry")
			(lambda (4:9-4:30)
				(args (ident (4:10-4:12) "fn"))
				(lambda (4:14-4:30)
					(args (ident (4:15-4:16) "x"))
					(lambda (4:18-4:30)
						(args (ident (4:19-4:20) "y"))
						(apply (4:22-4:30)
							(ident (4:22-4:24) "" "fn")
							(ident (4:25-4:26) "" "x")
							(ident (4:28-4:29) "" "y"))))))
		(decl (6:1-6:15)
			(ident (6:1-6:6) "main!")
			(lambda (6:9-6:15)
				(args (underscore))
				(record (6:13-6:15))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

curry : (a, b -> c)(a->b->c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (4:1-4:6)
				(pid 82)
				(ident "curry")))
		(def_expr
			(e_lambda (4:9-4:30)
				(args
					(p_assign (4:10-4:12)
						(pid 83)
						(ident "fn")))
				(e_lambda (4:14-4:30)
					(args
						(p_assign (4:15-4:16)
							(pid 84)
							(ident "x")))
					(e_lambda (4:18-4:30)
						(args
							(p_assign (4:19-4:20)
								(pid 85)
								(ident "y")))
						(e_call (4:22-4:30)
							(e_lookup (4:22-4:24) (pid 83))
							(e_lookup (4:25-4:26) (pid 84))
							(e_lookup (4:28-4:29) (pid 85)))))))
		(annotation (4:1-4:6)
			(signature 100)
			(declared_type
				(parens (3:9-3:20)
					(fn (3:10-3:19)
						(ty_var (3:10-3:11) "a")
						(ty_var (3:13-3:14) "b")
						(ty_var (3:18-3:19) "c")
						"false")))))
	(d_let
		(def_pattern
			(p_assign (6:1-6:6)
				(pid 103)
				(ident "main!")))
		(def_expr
			(e_lambda (6:9-6:15)
				(args (p_underscore (6:10-6:11) (pid 104)))
				(e_runtime_error (1:1-1:1) "not_implemented")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "curry" 102 (type "*"))
		(def "main!" 108 (type "*")))
	(expressions
		(expr (4:9-4:30) 92 (type "*"))
		(expr (6:9-6:15) 107 (type "*"))))
~~~