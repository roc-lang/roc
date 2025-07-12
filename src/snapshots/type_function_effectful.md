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
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_function_effectful.md:3:22:3:23
UNEXPECTED TOKEN IN EXPRESSION - type_function_effectful.md:3:26:3:28
INVALID STATEMENT - type_function_effectful.md:3:22:3:23
INVALID STATEMENT - type_function_effectful.md:3:24:3:25
INVALID STATEMENT - type_function_effectful.md:3:26:3:28
INVALID STATEMENT - type_function_effectful.md:3:29:3:30
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_effectful.md:3:22:3:23:**
```roc
runEffect! : (a => b), a => b
```
                     ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_effectful.md:3:26:3:28:**
```roc
runEffect! : (a => b), a => b
```
                         ^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_effectful.md:3:22:3:23:**
```roc
runEffect! : (a => b), a => b
```
                     ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_effectful.md:3:24:3:25:**
```roc
runEffect! : (a => b), a => b
```
                       ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_effectful.md:3:26:3:28:**
```roc
runEffect! : (a => b), a => b
```
                         ^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**type_function_effectful.md:3:29:3:30:**
```roc
runEffect! : (a => b), a => b
```
                            ^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:11),OpColon(3:12-3:13),OpenRound(3:14-3:15),LowerIdent(3:15-3:16),OpFatArrow(3:17-3:19),LowerIdent(3:20-3:21),CloseRound(3:21-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:25),OpFatArrow(3:26-3:28),LowerIdent(3:29-3:30),
LowerIdent(4:1-4:11),OpAssign(4:12-4:13),OpBar(4:14-4:15),LowerIdent(4:15-4:18),Comma(4:18-4:19),LowerIdent(4:20-4:21),OpBar(4:21-4:22),LowerIdent(4:23-4:26),NoSpaceOpenRound(4:26-4:27),LowerIdent(4:27-4:28),CloseRound(4:28-4:29),
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
		(s-type-anno @3.1-3.22 (name "runEffect!")
			(ty-fn @3.15-3.21
				(ty-var @3.15-3.15 (raw "a"))
				(ty-var @1.1-1.1 (raw "b"))))
		(e-malformed @3.22-3.23 (reason "expr_unexpected_token"))
		(e-ident @3.24-3.25 (raw "a"))
		(e-malformed @3.26-3.28 (reason "expr_unexpected_token"))
		(e-ident @3.29-3.30 (raw "b"))
		(s-decl @4.1-4.29
			(p-ident @4.1-4.11 (raw "runEffect!"))
			(e-lambda @4.14-4.29
				(args
					(p-ident @4.15-4.18 (raw "fn!"))
					(p-ident @4.20-4.21 (raw "x")))
				(e-apply @4.23-4.29
					(e-ident @4.23-4.26 (raw "fn!"))
					(e-ident @4.27-4.28 (raw "x")))))
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

runEffect! : (a => b)
a
b
runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.11 (ident "runEffect!"))
		(e-lambda @4.14-4.29
			(args
				(p-assign @4.15-4.18 (ident "fn!"))
				(p-assign @4.20-4.21 (ident "x")))
			(e-call @4.23-4.29
				(e-lookup-local @4.23-4.26
					(p-assign @4.15-4.18 (ident "fn!")))
				(e-lookup-local @4.27-4.28
					(p-assign @4.20-4.21 (ident "x"))))))
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
		(patt @4.1-4.11 (type "* -> *, * -> *"))
		(patt @6.1-6.6 (type "* -> {}")))
	(expressions
		(expr @4.14-4.29 (type "* -> *, * -> *"))
		(expr @6.9-6.15 (type "* -> {}"))))
~~~
