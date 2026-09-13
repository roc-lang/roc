# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_003.md:1:3:1:6
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:3:1:4
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:4:1:6
UNEXPECTED STATEMENT - fuzz_crash_003.md:1:6:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 1 3) (end 1 6))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_003.md") (start 1 3) (end 1 6) (annotation error) (line-text "= \"te"))))
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
			(annotated code "=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_003.md") (start 1 1) (end 1 2) (annotation error) (line-text "= \"te"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 3) (end 1 4))
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
			(annotated code "\"")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_003.md") (start 1 3) (end 1 4) (annotation error) (line-text "= \"te"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 4) (end 1 6))
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
			(annotated code "te")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_003.md") (start 1 4) (end 1 6) (annotation error) (line-text "= \"te"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 6) (end 1 6))
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
			(reflow "I reached the end of the file before this construct was complete.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_003.md") (start 1 6) (end 1 6) (annotation error) (line-text "= \"te")))))
~~~
# TOKENS
~~~zig
OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
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
