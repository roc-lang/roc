# META
~~~ini
description=Higher-order function with multiple type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_higher_order_multiple_vars.md:3:36:3:38
PARSE ERROR - type_higher_order_multiple_vars.md:3:39:3:40
PARSE ERROR - type_higher_order_multiple_vars.md:3:40:3:42
PARSE ERROR - type_higher_order_multiple_vars.md:3:43:3:45
PARSE ERROR - type_higher_order_multiple_vars.md:3:46:3:48
PARSE ERROR - type_higher_order_multiple_vars.md:3:48:3:49
# PROBLEMS
**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**type_higher_order_multiple_vars.md:3:36:3:38:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
                                   ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_higher_order_multiple_vars.md:3:39:3:40:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
                                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_higher_order_multiple_vars.md:3:40:3:42:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
                                       ^^


**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**type_higher_order_multiple_vars.md:3:43:3:45:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
                                          ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_higher_order_multiple_vars.md:3:46:3:48:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
                                             ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_higher_order_multiple_vars.md:3:48:3:49:**
```roc
compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
```
                                               ^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenRound(3:11-3:12),NamedUnderscore(3:12-3:14),OpArrow(3:15-3:17),NamedUnderscore(3:18-3:20),CloseRound(3:20-3:21),OpArrow(3:22-3:24),OpenRound(3:25-3:26),NamedUnderscore(3:26-3:28),OpArrow(3:29-3:31),NamedUnderscore(3:32-3:34),CloseRound(3:34-3:35),OpArrow(3:36-3:38),OpenRound(3:39-3:40),NamedUnderscore(3:40-3:42),OpArrow(3:43-3:45),NamedUnderscore(3:46-3:48),CloseRound(3:48-3:49),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:13),Comma(4:13-4:14),LowerIdent(4:15-4:16),OpBar(4:16-4:17),OpBar(4:18-4:19),LowerIdent(4:19-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:23),NoSpaceOpenRound(4:23-4:24),LowerIdent(4:24-4:25),NoSpaceOpenRound(4:25-4:26),LowerIdent(4:26-4:27),CloseRound(4:27-4:28),CloseRound(4:28-4:29),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),
EndOfFile(7:1-7:1),
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
		(s-type-anno @3.1-3.35 (name "compose")
			(ty-fn @3.11-3.35
				(ty-fn @3.12-3.20
					(underscore-ty-var @3.12-3.14 (raw "_b"))
					(underscore-ty-var @3.18-3.20 (raw "_c")))
				(ty-fn @3.26-3.34
					(underscore-ty-var @3.26-3.28 (raw "_a"))
					(underscore-ty-var @3.32-3.34 (raw "_b")))))
		(s-malformed @3.36-3.38 (tag "multi_arrow_needs_parens"))
		(s-malformed @3.39-3.40 (tag "statement_unexpected_token"))
		(s-malformed @3.40-3.42 (tag "statement_unexpected_token"))
		(s-malformed @3.43-3.45 (tag "multi_arrow_needs_parens"))
		(s-malformed @3.46-3.48 (tag "statement_unexpected_token"))
		(s-malformed @3.48-3.49 (tag "statement_unexpected_token"))
		(s-decl @4.1-4.29
			(p-ident @4.1-4.8 (raw "compose"))
			(e-lambda @4.11-4.29
				(args
					(p-ident @4.12-4.13 (raw "f"))
					(p-ident @4.15-4.16 (raw "g")))
				(e-lambda @4.18-4.29
					(args
						(p-ident @4.19-4.20 (raw "x")))
					(e-apply @4.22-4.29
						(e-ident @4.22-4.23 (raw "f"))
						(e-apply @4.24-4.28
							(e-ident @4.24-4.25 (raw "g"))
							(e-ident @4.26-4.27 (raw "x")))))))
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

compose : (_b -> _c) -> (_a -> _b)

compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "compose"))
		(e-lambda @4.11-4.29
			(args
				(p-assign @4.12-4.13 (ident "f"))
				(p-assign @4.15-4.16 (ident "g")))
			(e-closure @4.18-4.29
				(captures
					(capture @4.15-4.16 (ident "g"))
					(capture @4.12-4.13 (ident "f")))
				(e-lambda @4.18-4.29
					(args
						(p-assign @4.19-4.20 (ident "x")))
					(e-call @4.22-4.29
						(e-lookup-local @4.22-4.23
							(p-assign @4.12-4.13 (ident "f")))
						(e-call @4.24-4.28
							(e-lookup-local @4.24-4.25
								(p-assign @4.15-4.16 (ident "g")))
							(e-lookup-local @4.26-4.27
								(p-assign @4.19-4.20 (ident "x")))))))))
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
		(patt @4.1-4.8 (type "arg -> ret, _arg2 -> ret2 -> _arg3 -> ret3"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.11-4.29 (type "arg -> ret, _arg2 -> ret2 -> _arg3 -> ret3"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
