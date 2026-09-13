# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
# EXPECTED
LEADING ZERO - :0:0:0:0
UNEXPECTED STATEMENT - fuzz_crash_015.md:1:1:1:4
UNEXPECTED STATEMENT - fuzz_crash_015.md:1:4:1:6
UNEXPECTED STATEMENT - fuzz_crash_015.md:2:1:2:4
UNEXPECTED STATEMENT - fuzz_crash_015.md:3:1:3:4
UNEXPECTED STATEMENT - fuzz_crash_015.md:3:4:3:6
UNEXPECTED STATEMENT - fuzz_crash_015.md:4:1:4:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Leading Zero")
		(headline
			(reflow "Numbers cannot have leading zeros."))
		(document))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 4))
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
			(annotated code "0o0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_015.md") (start 1 1) (end 1 4) (annotation error) (line-text "0o0.0"))))
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
			(annotated code ".0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_015.md") (start 1 4) (end 1 6) (annotation error) (line-text "0o0.0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 1) (end 2 4))
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
			(annotated code "0_0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_015.md") (start 2 1) (end 2 4) (annotation error) (line-text "0_0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 1) (end 3 4))
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
			(annotated code "0u8")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_015.md") (start 3 1) (end 3 4) (annotation error) (line-text "0u8.0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 3 4) (end 3 6))
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
			(source-region (file "fuzz_crash_015.md") (start 3 4) (end 3 6) (annotation error) (line-text "0u8.0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 4 1) (end 4 3))
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
			(annotated code "0_")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_015.md") (start 4 1) (end 4 3) (annotation error) (line-text "0_")))))
~~~
# TOKENS
~~~zig
Int,NoSpaceDotInt,
Int,
Int,NoSpaceDotInt,
Int,
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
