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
AMBIGUOUS FUNCTION TYPE - type_function_simple.md:3:26:3:28
UNEXPECTED STATEMENT - type_function_simple.md:3:29:3:31
# PROBLEMS

┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  apply : (_a -> _b) -> _a -> _b                                            │
 │                           ‾‾                                               │
 └────────────────────────────────────────────── type_function_simple.md:3:26 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  apply : (_a -> _b) -> _a -> _b                                            │
 │                              ‾‾                                            │
 └────────────────────────────────────────────── type_function_simple.md:3:29 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `_b` here.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,NamedUnderscore,OpArrow,NamedUnderscore,CloseRound,OpArrow,NamedUnderscore,OpArrow,NamedUnderscore,
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
		(s-type-anno (name "apply")
			(ty-fn
				(ty-fn
					(underscore-ty-var (raw "_a"))
					(underscore-ty-var (raw "_b")))
				(underscore-ty-var (raw "_a"))))
		(s-malformed (tag "multi_arrow_needs_parens"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "fn"))
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "fn"))
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

apply : (_a -> _b) -> _a

apply = |fn, x| fn(x)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "fn"))
				(p-assign (ident "x")))
			(e-call (constraint-fn-var 30)
				(e-lookup-local
					(p-assign (ident "fn")))
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
		(patt (type "(a -> b), a -> b"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a -> b), a -> b"))
		(expr (type "_arg -> {}"))))
~~~
