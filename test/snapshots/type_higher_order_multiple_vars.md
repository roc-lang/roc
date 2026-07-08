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
AMBIGUOUS FUNCTION TYPE - type_higher_order_multiple_vars.md:3:36:3:38
UNEXPECTED STATEMENT - type_higher_order_multiple_vars.md:3:39:3:40
UNEXPECTED STATEMENT - type_higher_order_multiple_vars.md:3:40:3:42
AMBIGUOUS FUNCTION TYPE - type_higher_order_multiple_vars.md:3:43:3:45
UNEXPECTED STATEMENT - type_higher_order_multiple_vars.md:3:46:3:48
UNEXPECTED STATEMENT - type_higher_order_multiple_vars.md:3:48:3:49
# PROBLEMS

┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)                          │
 │                                     ‾‾                                     │
 └─────────────────────────────────── type_higher_order_multiple_vars.md:3:36 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)                          │
 │                                        ‾                                   │
 └─────────────────────────────────── type_higher_order_multiple_vars.md:3:39 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)                          │
 │                                         ‾‾                                 │
 └─────────────────────────────────── type_higher_order_multiple_vars.md:3:40 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `_a` here.


┌─────────────────────────┐
│ AMBIGUOUS FUNCTION TYPE ├─ I was parsing a function type, and multiple ─────┐
└┬────────────────────────┘  arrows need parentheses.                         │
 │                                                                            │
 │  compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)                          │
 │                                            ‾‾                              │
 └─────────────────────────────────── type_higher_order_multiple_vars.md:3:43 ┘

    Use parentheses to say whether the function returns another function or
    takes a function as an argument.

    For example:
        a -> (b -> c)
        (a -> b) -> c


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)                          │
 │                                               ‾‾                           │
 └─────────────────────────────────── type_higher_order_multiple_vars.md:3:46 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `_c` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)                          │
 │                                                 ‾                          │
 └─────────────────────────────────── type_higher_order_multiple_vars.md:3:48 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.

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
					(e-call (constraint-fn-var 43)
						(e-lookup-local
							(p-assign (ident "f")))
						(e-call (constraint-fn-var 40)
							(e-lookup-local
								(p-assign (ident "g")))
							(e-lookup-local
								(p-assign (ident "x")))))))))
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
		(patt (type "(a -> b), (c -> a) -> (c -> b)"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a -> b), (c -> a) -> (c -> b)"))
		(expr (type "_arg -> {}"))))
~~~
