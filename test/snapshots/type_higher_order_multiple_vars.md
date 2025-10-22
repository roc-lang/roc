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
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,NamedUnderscore,OpArrow,NamedUnderscore,CloseRound,OpArrow,OpenRound,NamedUnderscore,OpArrow,NamedUnderscore,CloseRound,OpArrow,OpenRound,NamedUnderscore,OpArrow,NamedUnderscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "compose")
			(ty-fn
				(ty-fn
					(underscore-ty-var (raw "_b"))
					(underscore-ty-var (raw "_c")))
				(ty-fn
					(underscore-ty-var (raw "_a"))
					(underscore-ty-var (raw "_b")))))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "compose"))
			(e-lambda
				(args
					(p-ident (raw "f"))
					(p-ident (raw "g")))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-apply
						(e-ident (raw "f"))
						(e-apply
							(e-ident (raw "g"))
							(e-ident (raw "x")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
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
		(p-assign (ident "compose"))
		(e-lambda
			(args
				(p-assign (ident "f"))
				(p-assign (ident "g")))
			(e-closure
				(captures
					(capture (ident "f"))
					(capture (ident "g")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-call
						(e-lookup-local
							(p-assign (ident "f")))
						(e-call
							(e-lookup-local
								(p-assign (ident "g")))
							(e-lookup-local
								(p-assign (ident "x")))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(s-type-anno (name "compose")
		(ty-fn (effectful false)
			(ty-parens
				(ty-fn (effectful false)
					(ty-rigid-var (name "_b"))
					(ty-rigid-var (name "_c"))))
			(ty-parens
				(ty-fn (effectful false)
					(ty-rigid-var (name "_a"))
					(ty-rigid-var-lookup (ty-rigid-var (name "_b"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b, c -> a -> c -> b"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> b, c -> a -> c -> b"))
		(expr (type "_arg -> {}"))))
~~~
