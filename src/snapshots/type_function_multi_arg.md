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
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_function_multi_arg.md:3:21:3:23
INVALID STATEMENT - type_function_multi_arg.md:3:21:3:23
INVALID STATEMENT - type_function_multi_arg.md:3:24:3:37
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **->** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_multi_arg.md:3:21:3:23:**
```roc
curry : (a, b -> c) -> (a -> b -> c)
```
                    ^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_multi_arg.md:3:21:3:23:**
```roc
curry : (a, b -> c) -> (a -> b -> c)
```
                    ^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_multi_arg.md:3:24:3:37:**
```roc
curry : (a, b -> c) -> (a -> b -> c)
```
                       ^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),OpenRound(3:9-3:10),LowerIdent(3:10-3:11),Comma(3:11-3:12),LowerIdent(3:13-3:14),OpArrow(3:15-3:17),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),OpArrow(3:21-3:23),OpenRound(3:24-3:25),LowerIdent(3:25-3:26),OpArrow(3:27-3:29),LowerIdent(3:30-3:31),OpArrow(3:32-3:34),LowerIdent(3:35-3:36),CloseRound(3:36-3:37),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:12),OpBar(4:12-4:13),OpBar(4:14-4:15),LowerIdent(4:15-4:16),OpBar(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:24),NoSpaceOpenRound(4:24-4:25),LowerIdent(4:25-4:26),Comma(4:26-4:27),LowerIdent(4:28-4:29),CloseRound(4:29-4:30),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file @1.1-6.15
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11 (text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.20 (name "curry")
			(ty-fn @3.10-3.19
				(ty-var @3.10-3.10 (raw "a"))
				(ty-var @1.1-1.1 (raw "b"))
				(ty-var @1.1-1.1 (raw "c"))))
		(e-malformed @3.21-3.23 (reason "expr_unexpected_token"))
		(e-tuple @3.24-3.37
			(e-local-dispatch @3.25-3.36
				(e-local-dispatch @3.25-3.31
					(e-ident @3.25-3.26 (raw "a"))
					(e-ident @1.1-1.1 (raw "b")))
				(e-ident @1.1-1.1 (raw "c"))))
		(s-decl @4.1-4.30
			(p-ident @4.1-4.6 (raw "curry"))
			(e-lambda @4.9-4.30
				(args
					(p-ident @4.10-4.12 (raw "fn")))
				(e-lambda @4.14-4.30
					(args
						(p-ident @4.15-4.16 (raw "x")))
					(e-lambda @4.18-4.30
						(args
							(p-ident @4.19-4.20 (raw "y")))
						(e-apply @4.22-4.30
							(e-ident @4.22-4.24 (raw "fn"))
							(e-ident @4.25-4.26 (raw "x"))
							(e-ident @4.28-4.29 (raw "y")))))))
		(s-decl @6.1-6.15
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.15
				(args
					(p-underscore))
				(e-record @6.13-6.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

curry : (a, b -> c)
(a->b->c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "curry"))
		(e-lambda @4.9-4.30
			(args
				(p-assign @4.10-4.12 (ident "fn")))
			(e-lambda @4.14-4.30
				(args
					(p-assign @4.15-4.16 (ident "x")))
				(e-lambda @4.18-4.30
					(args
						(p-assign @4.19-4.20 (ident "y")))
					(e-call @4.22-4.30
						(e-lookup-local @4.22-4.24
							(p-assign @4.10-4.12 (ident "fn")))
						(e-lookup-local @4.25-4.26
							(p-assign @4.15-4.16 (ident "x")))
						(e-lookup-local @4.28-4.29
							(p-assign @4.19-4.20 (ident "y"))))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-lambda @6.9-6.15
			(args
				(p-underscore @6.10-6.11))
			(e-empty_record @6.13-6.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "arg, arg2 -> d -> arg -> arg2 -> d"))
		(patt @6.1-6.6 (type "arg3 -> {}")))
	(expressions
		(expr @4.9-4.30 (type "arg, arg2 -> d -> arg -> arg2 -> d"))
		(expr @6.9-6.15 (type "arg3 -> {}"))))
~~~
