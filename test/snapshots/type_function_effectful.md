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
AMBIGUOUS FUNCTION TYPE - type_function_effectful.md:3:31:3:33
UNEXPECTED STATEMENT - type_function_effectful.md:3:34:3:36
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 3 31) (end 3 33))
		(headline
			(reflow "I was parsing a function type, and multiple arrows need parentheses."))
		(document
			(reflow "Use parentheses to say whether the function returns another function or takes a function as an argument.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a -> (b -> c)")
			(line-break)
			(indent 1)
			(text "(a -> b) -> c")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "type_function_effectful.md") (start 3 31) (end 3 33) (annotation error) (line-text "runEffect! : (_a => _b) -> _a => _b"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 34) (end 3 36))
		(headline
			(reflow "I was parsing a statement, and this token cannot start a statement here."))
		(document
			(reflow "Statements can be declarations, type annotations, imports, expectations, returns, crashes, loops, or expression statements inside a block.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "answer = 42")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "_b")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "type_function_effectful.md") (start 3 34) (end 3 36) (annotation error) (line-text "runEffect! : (_a => _b) -> _a => _b")))))
~~~
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
			(e-call (constraint-fn-var 221)
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
		(patt (type "(a -> b), a -> b"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "(a -> b), a -> b"))
		(expr (type "_arg -> {}"))))
~~~
