# META
~~~ini
description=Higher-order function with multiple type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

compose : (b -> c), (a -> b) -> (a -> c)
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **, (** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_higher_order_multiple_vars.md:3:19:3:22:**
```roc
compose : (b -> c), (a -> b) -> (a -> c)
```


**PARSE ERROR**
A parsing error occurred: `expr_arrow_expects_ident`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_higher_order_multiple_vars.md:3:33:3:35:**
```roc
compose : (b -> c), (a -> b) -> (a -> c)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token  is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_higher_order_multiple_vars.md:3:40:3:40:**
```roc
compose : (b -> c), (a -> b) -> (a -> c)
```


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

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenRound(3:11-3:12),LowerIdent(3:12-3:13),OpArrow(3:14-3:16),LowerIdent(3:17-3:18),CloseRound(3:18-3:19),Comma(3:19-3:20),OpenRound(3:21-3:22),LowerIdent(3:22-3:23),OpArrow(3:24-3:26),LowerIdent(3:27-3:28),CloseRound(3:28-3:29),OpArrow(3:30-3:32),OpenRound(3:33-3:34),LowerIdent(3:34-3:35),OpArrow(3:36-3:38),LowerIdent(3:39-3:40),CloseRound(3:40-3:41),Newline(1:1-1:1),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:13),Comma(4:13-4:14),LowerIdent(4:15-4:16),OpBar(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:23),NoSpaceOpenRound(4:23-4:24),LowerIdent(4:24-4:25),NoSpaceOpenRound(4:25-4:26),LowerIdent(4:26-4:27),CloseRound(4:27-4:28),CloseRound(4:28-4:29),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file @1-1-6-15
	(app @1-1-1-53
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-53 (name "pf")
			(e-string @1-28-1-51
				(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))
		(packages @1-13-1-53
			(record-field @1-15-1-53 (name "pf")
				(e-string @1-28-1-51
					(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3-1-3-20 (name "compose")
			(ty-fn @3-12-3-18
				(ty-var @3-12-3-13 (raw "b"))
				(ty-var @3-17-3-18 (raw "c"))))
		(e-malformed @3-19-3-22 (reason "expr_unexpected_token"))
		(e-malformed @3-33-3-35 (reason "expr_arrow_expects_ident"))
		(e-local-dispatch @3-34-3-41
			(e-ident @3-34-3-35 (qaul "") (raw "a"))
			(e-ident @3-39-3-40 (qaul "") (raw "c")))
		(e-malformed @1-1-1-1 (reason "expr_unexpected_token"))
		(s-decl @4-1-4-29
			(p-ident @4-1-4-8 (raw "compose"))
			(e-lambda @4-11-4-29
				(args
					(p-ident @4-12-4-13 (raw "f"))
					(p-ident @4-15-4-16 (raw "g")))
				(e-lambda @4-18-4-29
					(args
						(p-ident @4-19-4-20 (raw "x")))
					(e-apply @4-22-4-29
						(e-ident @4-22-4-23 (qaul "") (raw "f"))
						(e-apply @4-24-4-28
							(e-ident @4-24-4-25 (qaul "") (raw "g"))
							(e-ident @4-26-4-27 (qaul "") (raw "x")))))))
		(s-decl @6-1-6-15
			(p-ident @6-1-6-6 (raw "main!"))
			(e-lambda @6-9-6-15
				(args
					(p-underscore))
				(e-record @6-13-6-15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

compose : (b -> c)a->c
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 100)
		(p-assign @4-1-4-8 (ident "compose") (id 82))
		(e-lambda @4-11-4-29 (id 92)
			(args
				(p-assign @4-12-4-13 (ident "f") (id 83))
				(p-assign @4-15-4-16 (ident "g") (id 84)))
			(e-lambda @4-18-4-29
				(args
					(p-assign @4-19-4-20 (ident "x") (id 85)))
				(e-call @4-22-4-29
					(e-lookup-local @4-22-4-23
						(pattern (id 83)))
					(e-call @4-24-4-28
						(e-lookup-local @4-24-4-25
							(pattern (id 84)))
						(e-lookup-local @4-26-4-27
							(pattern (id 85)))))))
		(annotation @4-1-4-8 (signature 98) (id 99)
			(declared-type
				(ty-parens @3-11-3-19
					(ty-fn @3-12-3-18 (effectful false)
						(ty-var @3-12-3-13 (name "b"))
						(ty-var @3-17-3-18 (name "c")))))))
	(d-let (id 105)
		(p-assign @6-1-6-6 (ident "main!") (id 101))
		(e-lambda @6-9-6-15 (id 104)
			(args
				(p-underscore @6-10-6-11 (id 102)))
			(e-empty_record @6-13-6-15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "compose") (type "*"))
		(def (name "main!") (type "*")))
	(expressions
		(expr @4-11-4-29 (type "*"))
		(expr @6-9-6-15 (type "*"))))
~~~