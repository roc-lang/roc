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
                ^^^


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_function_simple.md:4:1:4:6:**
```roc
apply = |fn, x| fn(x)
```
^^^^^

It is of type:
    _a -> b_

But you are trying to use it as:
    _*, * ? *_

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
(file @1.1-6.15
	(app @1.1-1.53
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.53 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.53 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.18 (name "apply")
			(ty-fn @3.10-3.16
				(ty-var @3.10-3.11 (raw "a"))
				(ty-var @3.15-3.16 (raw "b"))))
		(e-malformed @3.17-3.20 (reason "expr_unexpected_token"))
		(e-local-dispatch @3.19-4.6
			(e-ident @3.19-3.20 (qaul "") (raw "a"))
			(e-ident @3.24-3.25 (qaul "") (raw "b")))
		(s-decl @4.1-4.22
			(p-ident @4.1-4.6 (raw "apply"))
			(e-lambda @4.9-4.22
				(args
					(p-ident @4.10-4.12 (raw "fn"))
					(p-ident @4.14-4.15 (raw "x")))
				(e-apply @4.17-4.22
					(e-ident @4.17-4.19 (qaul "") (raw "fn"))
					(e-ident @4.20-4.21 (qaul "") (raw "x")))))
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

apply : (a -> b)a->b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "apply"))
		(e-lambda @4.9-4.22
			(args
				(p-assign @4.10-4.12 (ident "fn"))
				(p-assign @4.14-4.15 (ident "x")))
			(e-call @4.17-4.22
				(e-lookup-local @4.17-4.19
					(pattern @4.10-4.12))
				(e-lookup-local @4.20-4.21
					(pattern @4.14-4.15))))
		(annotation @4.1-4.6
			(declared-type
				(ty-parens @3.9-3.17
					(ty-fn @3.10-3.16 (effectful false)
						(ty-var @3.10-3.11 (name "a"))
						(ty-var @3.15-3.16 (name "b")))))))
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
		(patt @4.1-4.6 (type "Error"))
		(patt @6.1-6.6 (type "* ? {}")))
	(expressions
		(expr @4.9-4.22 (type "Error"))
		(expr @6.9-6.15 (type "* ? {}"))))
~~~
