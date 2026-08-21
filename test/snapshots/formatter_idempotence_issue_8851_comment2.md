# META
~~~ini
description=Formatter idempotence test for issue 8851 comment 2 - chained empty parens with tuple dispatch
type=snippet
~~~
# SOURCE
~~~roc
a=()->b()()()
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - formatter_idempotence_issue_8851_comment2.md:1:3:1:5
NAME NOT IN SCOPE - formatter_idempotence_issue_8851_comment2.md:1:7:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Empty Tuple Not Allowed")
		(region (start 1 3) (end 1 5))
		(headline
			(reflow "I am part way through parsing this tuple, but it is empty."))
		(document
			(source-region (file "formatter_idempotence_issue_8851_comment2.md") (start 1 3) (end 1 5) (annotation error) (line-text "a=()->b()()()"))
			(line-break)
			(reflow "If you want to represent nothing, try using an empty record: ")
			(annotated code "{}")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 7) (end 1 8))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "b")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "formatter_idempotence_issue_8851_comment2.md") (start 1 7) (end 1 8) (annotation error) (line-text "a=()->b()()()")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,CloseRound,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpenRound,CloseRound,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-arrow-call
				(e-tuple)
				(e-apply
					(e-apply
						(e-apply
							(e-ident (raw "b")))))))))
~~~
# FORMATTED
~~~roc
a = () |> b()()()
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
