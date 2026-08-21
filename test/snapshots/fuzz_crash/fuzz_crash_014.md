# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_014.md:1:1:1:3
UNEXPECTED STATEMENT - fuzz_crash_014.md:1:3:1:5
UNEXPECTED STATEMENT - fuzz_crash_014.md:2:1:2:6
UNEXPECTED STATEMENT - fuzz_crash_014.md:3:1:3:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 3))
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
			(annotated code "0b")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_014.md") (start 1 1) (end 1 3) (annotation error) (line-text "0b.0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 3) (end 1 5))
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
			(annotated code ".0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_014.md") (start 1 3) (end 1 5) (annotation error) (line-text "0b.0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 1) (end 2 6))
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
			(annotated code "0bu22")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_014.md") (start 2 1) (end 2 6) (annotation error) (line-text "0bu22"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 1) (end 3 5))
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
			(annotated code "0u22")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_014.md") (start 3 1) (end 3 5) (annotation error) (line-text "0u22")))))
~~~
# TOKENS
~~~zig
MalformedNumberNoDigits,NoSpaceDotInt,
MalformedNumberNoDigits,
MalformedNumberBadSuffix,
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
