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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 3 36) (end 3 38))
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
			(source-region (file "type_higher_order_multiple_vars.md") (start 3 36) (end 3 38) (annotation error) (line-text "compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 39) (end 3 40))
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
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "type_higher_order_multiple_vars.md") (start 3 39) (end 3 40) (annotation error) (line-text "compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 40) (end 3 42))
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
			(annotated code "_a")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "type_higher_order_multiple_vars.md") (start 3 40) (end 3 42) (annotation error) (line-text "compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)"))))
	(report
		(severity runtime_error)
		(title "Ambiguous Function Type")
		(region (start 3 43) (end 3 45))
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
			(source-region (file "type_higher_order_multiple_vars.md") (start 3 43) (end 3 45) (annotation error) (line-text "compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 46) (end 3 48))
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
			(annotated code "_c")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "type_higher_order_multiple_vars.md") (start 3 46) (end 3 48) (annotation error) (line-text "compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 48) (end 3 49))
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
			(annotated code ")")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "type_higher_order_multiple_vars.md") (start 3 48) (end 3 49) (annotation error) (line-text "compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)")))))
~~~
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
					(e-call (constraint-fn-var 234)
						(e-lookup-local
							(p-assign (ident "f")))
						(e-call (constraint-fn-var 231)
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
