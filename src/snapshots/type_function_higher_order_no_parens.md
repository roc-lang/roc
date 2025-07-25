# META
~~~ini
description=Higher-order function type annotation without outer parentheses
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

foo : (Str -> Str) -> Str
foo = |f| f("hello")

main! = |_| foo(|x| x)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **-> Str** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_higher_order_no_parens.md:3:20:3:26:**
```roc
foo : (Str -> Str) -> Str
```
                   ^^^^^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

Here is the problematic code:
**type_function_higher_order_no_parens.md:3:23:3:23:**
```roc
foo : (Str -> Str) -> Str
```
                      


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= |** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_function_higher_order_no_parens.md:4:5:4:8:**
```roc
foo = |f| f("hello")
```
    ^^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

**type_function_higher_order_no_parens.md:6:13:6:16:**
```roc
main! = |_| foo(|x| x)
```
            ^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),OpenRound(3:7-3:8),UpperIdent(3:8-3:11),OpArrow(3:12-3:14),UpperIdent(3:15-3:18),CloseRound(3:18-3:19),OpArrow(3:20-3:22),UpperIdent(3:23-3:26),Newline(1:1-1:1),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),OpBar(4:7-4:8),LowerIdent(4:8-4:9),OpBar(4:9-4:10),LowerIdent(4:11-4:12),NoSpaceOpenRound(4:12-4:13),StringStart(4:13-4:14),StringPart(4:14-4:19),StringEnd(4:19-4:20),CloseRound(4:20-4:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:16),NoSpaceOpenRound(6:16-6:17),OpBar(6:17-6:18),LowerIdent(6:18-6:19),OpBar(6:19-6:20),LowerIdent(6:21-6:22),CloseRound(6:22-6:23),EndOfFile(6:23-6:23),
~~~
# PARSE
~~~clojure
(file @1.1-6.23
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
		(s-type-anno @3.1-3.22 (name "foo")
			(ty-fn @3.8-3.18
				(ty @3.8-3.11 (name "Str"))
				(ty @3.15-3.18 (name "Str"))))
		(e-malformed @3.20-3.26 (reason "expr_unexpected_token"))
		(s-malformed @3.23-4.6 (tag "expected_colon_after_type_annotation"))
		(e-malformed @4.5-4.8 (reason "expr_unexpected_token"))
		(e-lambda @4.7-4.21
			(args
				(p-ident @4.8-4.9 (raw "f")))
			(e-apply @4.11-4.21
				(e-ident @4.11-4.12 (raw "f"))
				(e-string @4.13-4.20
					(e-string-part @4.14-4.19 (raw "hello")))))
		(s-decl @6.1-6.23
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.23
				(args
					(p-underscore))
				(e-apply @6.13-6.23
					(e-ident @6.13-6.16 (raw "foo"))
					(e-lambda @6.17-6.22
						(args
							(p-ident @6.18-6.19 (raw "x")))
						(e-ident @6.21-6.22 (raw "x"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

foo : (Str -> Str)|f| f("hello")

main! = |_| foo(|x| x)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-lambda @6.9-6.23
			(args
				(p-underscore @6.10-6.11))
			(e-call @6.13-6.23
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lambda @6.17-6.22
					(args
						(p-assign @6.18-6.19 (ident "x")))
					(e-lookup-local @6.21-6.22
						(pattern @6.18-6.19)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "* -> *")))
	(expressions
		(expr @6.9-6.23 (type "* -> *"))))
~~~
