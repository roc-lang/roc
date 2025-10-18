# META
~~~ini
description=Effectful function type with fat arrow syntax
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

runEffect! : (_a => _b) -> _a => _b
runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_function_effectful.md:3:31:3:33
PARSE ERROR - type_function_effectful.md:3:34:3:36
# PROBLEMS
**PARSE ERROR**
Function types with multiple arrows need parentheses.

Instead of writing **a -> b -> c**, use parentheses to clarify which you mean:
        a -> (b -> c) for a **curried** function (a function that **returns** another function)
        (a -> b) -> c for a **higher-order** function (a function that **takes** another function)

**type_function_effectful.md:3:31:3:33:**
```roc
runEffect! : (_a => _b) -> _a => _b
```
                              ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**type_function_effectful.md:3:34:3:36:**
```roc
runEffect! : (_a => _b) -> _a => _b
```
                                 ^^


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,NamedUnderscore,OpFatArrow,NamedUnderscore,CloseRound,OpArrow,NamedUnderscore,OpFatArrow,NamedUnderscore,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
		(s-type-anno (name "runEffect!")
			(ty-fn
				(ty-fn
					(underscore-ty-var (raw "_a"))
					(underscore-ty-var (raw "_b")))
				(underscore-ty-var (raw "_a"))))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "runEffect!"))
			(e-lambda
				(args
					(p-ident (raw "fn!"))
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "fn!"))
					(e-ident (raw "x")))))
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

runEffect! : (_a => _b) -> _a

runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "runEffect!"))
		(e-lambda
			(args
				(p-assign (ident "fn!"))
				(p-assign (ident "x")))
			(e-call
				(e-lookup-local
					(p-assign (ident "fn!")))
				(e-lookup-local
					(p-assign (ident "x"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b, a -> b"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "a -> b, a -> b"))
		(expr (type "_arg -> {}"))))
~~~
