# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
)
 
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_050.md:1:1:1:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 2))
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
			(source-region (file "fuzz_crash_050.md") (start 1 1) (end 1 2) (annotation error) (line-text ")")))))
~~~
# TOKENS
~~~zig
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
