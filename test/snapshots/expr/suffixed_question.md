# META
~~~ini
description=suffixed_question
type=expr
~~~
# SOURCE
~~~roc
Stdout.line???
~~~
# EXPECTED
UNEXPECTED EXPRESSION SYNTAX - suffixed_question.md:1:14:1:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Expression Syntax")
		(region (start 1 14) (end 1 15))
		(headline
			(reflow "I was parsing an expression, and this token cannot start an expression here."))
		(document
			(reflow "Expressions can be names, literals, tags, records, lists, tuples, lambdas, blocks, conditionals, matches, or function calls.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "add(1, 2)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "?")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "suffixed_question.md") (start 1 14) (end 1 15) (annotation error) (line-text "Stdout.line???")))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceDotLowerIdent,OpDoubleQuestion,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "??")
	(e-ident (raw "Stdout.line"))
	(e-malformed (reason "expr_unexpected_token")))
~~~
# FORMATTED
~~~roc
Stdout.line ??
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "qualified_ident_does_not_exist")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-lookup-local
						(p-assign (ident "#ok")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-nominal-external (builtin)
							(p-applied-tag))))
				(value
					(e-runtime-error (tag "expr_syntax_error")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
