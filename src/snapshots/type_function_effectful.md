# META
~~~ini
description=Effectful function type with fat arrow syntax
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

runEffect! : (a => b), a => b
runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **, a** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_effectful.md:3:22:3:25:**
```roc
runEffect! : (a => b), a => b
```
                     ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> b** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_effectful.md:3:26:3:30:**
```roc
runEffect! : (a => b), a => b
```
                         ^^^^


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_function_effectful.md:4:1:4:11:**
```roc
runEffect! = |fn!, x| fn!(x)
```
^^^^^^^^^^

It is of type:
    _a => b_

But you are trying to use it as:
    _*, * ? *_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:11),OpColon(3:12-3:13),OpenRound(3:14-3:15),LowerIdent(3:15-3:16),OpFatArrow(3:17-3:19),LowerIdent(3:20-3:21),CloseRound(3:21-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:25),OpFatArrow(3:26-3:28),LowerIdent(3:29-3:30),Newline(1:1-1:1),
LowerIdent(4:1-4:11),OpAssign(4:12-4:13),OpBar(4:14-4:15),LowerIdent(4:15-4:18),Comma(4:18-4:19),LowerIdent(4:20-4:21),OpBar(4:21-4:22),LowerIdent(4:23-4:26),NoSpaceOpenRound(4:26-4:27),LowerIdent(4:27-4:28),CloseRound(4:28-4:29),Newline(1:1-1:1),
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
		(s-type-anno @3.1-3.23 (name "runEffect!")
			(ty-fn @3.15-3.21
				(ty-var @3.15-3.16 (raw "a"))
				(ty-var @3.20-3.21 (raw "b"))))
		(e-malformed @3.22-3.25 (reason "expr_unexpected_token"))
		(e-ident @3.24-3.25 (qaul "") (raw "a"))
		(e-malformed @3.26-3.30 (reason "expr_unexpected_token"))
		(e-ident @3.29-3.30 (qaul "") (raw "b"))
		(s-decl @4.1-4.29
			(p-ident @4.1-4.11 (raw "runEffect!"))
			(e-lambda @4.14-4.29
				(args
					(p-ident @4.15-4.18 (raw "fn!"))
					(p-ident @4.20-4.21 (raw "x")))
				(e-apply @4.23-4.29
					(e-ident @4.23-4.26 (qaul "") (raw "fn!"))
					(e-ident @4.27-4.28 (qaul "") (raw "x")))))
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

runEffect! : (a => b)ab
runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 99)
		(p-assign @4.1-4.11 (ident "runEffect!") (id 83))
		(e-lambda @4.14-4.29 (id 90)
			(args
				(p-assign @4.15-4.18 (ident "fn!") (id 84))
				(p-assign @4.20-4.21 (ident "x") (id 85)))
			(e-call @4.23-4.29
				(e-lookup-local @4.23-4.26
					(pattern (id 84)))
				(e-lookup-local @4.27-4.28
					(pattern (id 85)))))
		(annotation @4.1-4.11 (signature 97) (id 98)
			(declared-type
				(ty-parens @3.14-3.22
					(ty-fn @3.15-3.21 (effectful true)
						(ty-var @3.15-3.16 (name "a"))
						(ty-var @3.20-3.21 (name "b")))))))
	(d-let (id 105)
		(p-assign @6.1-6.6 (ident "main!") (id 100))
		(e-lambda @6.9-6.15 (id 104)
			(args
				(p-underscore @6.10-6.11 (id 101)))
			(e-empty_record @6.13-6.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "runEffect!") (def_var 99) (type "Error"))
		(d_assign (name "main!") (def_var 105) (type "* ? {}")))
	(expressions
		(expr @4.14-4.29 (type "Error"))
		(expr @6.9-6.15 (type "* ? {}"))))
~~~
