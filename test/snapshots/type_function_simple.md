# META
~~~ini
description=Simple function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

apply : (_a -> _b) -> _a -> _b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_function_simple.md:3:26:3:28
PARSE ERROR - type_function_simple.md:3:29:3:31
# PROBLEMS
**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**type_function_simple.md:3:26:3:28:**
```roc
apply : (_a -> _b) -> _a -> _b
```
                         ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_function_simple.md:3:29:3:31:**
```roc
apply : (_a -> _b) -> _a -> _b
```
                            ^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),OpenRound(3:9-3:10),NamedUnderscore(3:10-3:12),OpArrow(3:13-3:15),NamedUnderscore(3:16-3:18),CloseRound(3:18-3:19),OpArrow(3:20-3:22),NamedUnderscore(3:23-3:25),OpArrow(3:26-3:28),NamedUnderscore(3:29-3:31),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:12),Comma(4:12-4:13),LowerIdent(4:14-4:15),OpBar(4:15-4:16),LowerIdent(4:17-4:19),NoSpaceOpenRound(4:19-4:20),LowerIdent(4:20-4:21),CloseRound(4:21-4:22),
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
		(s-type-anno @3.1-3.25 (name "apply")
			(ty-fn @3.9-3.25
				(ty-fn @3.10-3.18
					(underscore-ty-var @3.10-3.12 (raw "_a"))
					(underscore-ty-var @3.16-3.18 (raw "_b")))
				(underscore-ty-var @3.23-3.25 (raw "_a"))))
		(s-malformed @3.26-3.28 (tag "multi_arrow_needs_parens"))
		(s-malformed @3.29-3.31 (tag "statement_unexpected_token"))
		(s-decl @4.1-4.22
			(p-ident @4.1-4.6 (raw "apply"))
			(e-lambda @4.9-4.22
				(args
					(p-ident @4.10-4.12 (raw "fn"))
					(p-ident @4.14-4.15 (raw "x")))
				(e-apply @4.17-4.22
					(e-ident @4.17-4.19 (raw "fn"))
					(e-ident @4.20-4.21 (raw "x")))))
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

apply : (_a -> _b) -> _a

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
				(e-lookup-local @4.20-4.21
					(p-assign @4.14-4.15 (ident "x"))))))
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
		(patt @4.1-4.6 (type "a -> b, a -> b"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.9-4.22 (type "a -> b, a -> b"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
