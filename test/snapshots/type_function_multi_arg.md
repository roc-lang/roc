# META
~~~ini
description=Multi-argument function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

curry : (_a, _b -> _c) -> (_a -> _b -> _c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# EXPECTED
EXPECTED CLOSING PARENTHESIS - type_function_multi_arg.md:3:27:3:28
UNEXPECTED STATEMENT - type_function_multi_arg.md:3:40:3:42
UNEXPECTED STATEMENT - type_function_multi_arg.md:3:42:3:43
MALFORMED TYPE - type_function_multi_arg.md:3:27:3:39
# PROBLEMS

┌──────────────────────────────┐
│ EXPECTED CLOSING PARENTHESIS ├─ I was parsing a parenthesized type, and I ──┐
└┬─────────────────────────────┘  expected `)`.                               │
 │                                                                            │
 │  curry : (_a, _b -> _c) -> (_a -> _b -> _c)                                │
 │                            ‾                                               │
 └─────────────────────────────────────────── type_function_multi_arg.md:3:27 ┘

    Close the parenthesized type after the final type expression.

    For example:
        (Str -> U64)

    I found `(` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  curry : (_a, _b -> _c) -> (_a -> _b -> _c)                                │
 │                                         ‾‾                                 │
 └─────────────────────────────────────────── type_function_multi_arg.md:3:40 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `_c` here.


┌──────────────────────┐
│ UNEXPECTED STATEMENT ├─ I was parsing a statement, and this token cannot ───┐
└┬─────────────────────┘  start a statement here.                             │
 │                                                                            │
 │  curry : (_a, _b -> _c) -> (_a -> _b -> _c)                                │
 │                                           ‾                                │
 └─────────────────────────────────────────── type_function_multi_arg.md:3:42 ┘

    Statements can be declarations, type annotations, imports, expectations,
    returns, crashes, loops, or expression statements inside a block.

    For example:
        answer = 42

    I found `)` here.
    This closes the current construct, so the parser was looking for the
    missing item before it.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  curry : (_a, _b -> _c) -> (_a -> _b -> _c)                                │
 │                            ‾‾‾‾‾‾‾‾‾‾‾‾                                    │
 └─────────────────────────────────────────── type_function_multi_arg.md:3:27 ┘


# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,OpenRound,NamedUnderscore,Comma,NamedUnderscore,OpArrow,NamedUnderscore,CloseRound,OpArrow,OpenRound,NamedUnderscore,OpArrow,NamedUnderscore,OpArrow,NamedUnderscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
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
		(s-type-anno (name "curry")
			(ty-fn
				(ty-fn
					(underscore-ty-var (raw "_a"))
					(underscore-ty-var (raw "_b"))
					(underscore-ty-var (raw "_c")))
				(ty-malformed (tag "expected_ty_anno_close_round"))))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-decl
			(p-ident (raw "curry"))
			(e-lambda
				(args
					(p-ident (raw "fn")))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-lambda
						(args
							(p-ident (raw "y")))
						(e-apply
							(e-ident (raw "fn"))
							(e-ident (raw "x"))
							(e-ident (raw "y")))))))
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

curry : (_a, _b -> _c) -> 

curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "curry"))
		(e-lambda
			(args
				(p-assign (ident "fn")))
			(e-closure
				(captures
					(capture (ident "fn")))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-closure
						(captures
							(capture (ident "fn"))
							(capture (ident "x")))
						(e-lambda
							(args
								(p-assign (ident "y")))
							(e-call (constraint-fn-var 203)
								(e-lookup-local
									(p-assign (ident "fn")))
								(e-lookup-local
									(p-assign (ident "x")))
								(e-lookup-local
									(p-assign (ident "y"))))))))))
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
		(patt (type "(a, b -> c) -> (a -> (b -> c))"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a, b -> c) -> (a -> (b -> c))"))
		(expr (type "_arg -> {}"))))
~~~
