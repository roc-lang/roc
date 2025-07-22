# META
~~~ini
description=Multi-argument function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

curry : (_a, _b -> _c) -> (_a -> _b -> _c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_function_multi_arg.md:3:24:3:26
PARSE ERROR - type_function_multi_arg.md:3:34:3:36
PARSE ERROR - type_function_multi_arg.md:3:42:3:43
INVALID STATEMENT - type_function_multi_arg.md:3:24:3:26
INVALID STATEMENT - type_function_multi_arg.md:3:27:3:43
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **->** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_multi_arg.md:3:24:3:26:**
```roc
curry : (_a, _b -> _c) -> (_a -> _b -> _c)
```
                       ^^


**PARSE ERROR**
A parsing error occurred: `expr_arrow_expects_ident`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_function_multi_arg.md:3:34:3:36:**
```roc
curry : (_a, _b -> _c) -> (_a -> _b -> _c)
```
                                 ^^


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_function_multi_arg.md:3:42:3:43:**
```roc
curry : (_a, _b -> _c) -> (_a -> _b -> _c)
```
                                         ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_multi_arg.md:3:24:3:26:**
```roc
curry : (_a, _b -> _c) -> (_a -> _b -> _c)
```
                       ^^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_multi_arg.md:3:27:3:43:**
```roc
curry : (_a, _b -> _c) -> (_a -> _b -> _c)
```
                          ^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),OpenRound(3:9-3:10),NamedUnderscore(3:10-3:12),Comma(3:12-3:13),NamedUnderscore(3:14-3:16),OpArrow(3:17-3:19),NamedUnderscore(3:20-3:22),CloseRound(3:22-3:23),OpArrow(3:24-3:26),OpenRound(3:27-3:28),NamedUnderscore(3:28-3:30),OpArrow(3:31-3:33),NamedUnderscore(3:34-3:36),OpArrow(3:37-3:39),NamedUnderscore(3:40-3:42),CloseRound(3:42-3:43),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:12),OpBar(4:12-4:13),OpBar(4:14-4:15),LowerIdent(4:15-4:16),OpBar(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:24),NoSpaceOpenRound(4:24-4:25),LowerIdent(4:25-4:26),Comma(4:26-4:27),LowerIdent(4:28-4:29),CloseRound(4:29-4:30),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file @1.1-6.15
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.23 (name "curry")
			(ty-fn @3.10-3.22
				(underscore-ty-var @3.10-3.12 (raw "_a"))
				(underscore-ty-var @3.14-3.16 (raw "_b"))
				(underscore-ty-var @3.20-3.22 (raw "_c"))))
		(e-malformed @3.24-3.26 (reason "expr_unexpected_token"))
		(e-malformed @3.42-3.43 (reason "expected_expr_close_round_or_comma"))
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

curry : (_a, _b -> _c)

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
					(captures
						(capture @4.10-4.12 (ident "fn"))
						(capture @4.15-4.16 (ident "x")))
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
		(patt @4.1-4.6 (type "_arg, _arg2 -> ret -> _arg3 -> _arg4 -> ret2"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.9-4.30 (type "_arg, _arg2 -> ret -> _arg3 -> _arg4 -> ret2"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
