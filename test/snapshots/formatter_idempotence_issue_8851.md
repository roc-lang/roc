# META
~~~ini
description=Formatter idempotence test for issue 8851 - chained empty parens with static dispatch
type=snippet
~~~
# SOURCE
~~~roc
a = 0->b().c()
~~~
# EXPECTED
NAME NOT IN SCOPE - formatter_idempotence_issue_8851.md:1:8:1:9
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 8) (end 1 9))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "b")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "formatter_idempotence_issue_8851.md") (start 1 8) (end 1 9) (annotation error) (line-text "a = 0->b().c()")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-method-call (method ".c")
				(receiver
					(e-arrow-call
						(e-int (raw "0"))
						(e-apply
							(e-ident (raw "b")))))
				(args)))))
~~~
# FORMATTED
~~~roc
a = (0 |> b).c()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
