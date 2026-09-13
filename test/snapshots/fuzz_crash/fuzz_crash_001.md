# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_001.md:1:1:1:3
UNEXPECTED STATEMENT - fuzz_crash_001.md:1:3:1:4
UNEXPECTED STATEMENT - fuzz_crash_001.md:1:4:1:5
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
			(annotated code "mo")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_001.md") (start 1 1) (end 1 3) (annotation error) (line-text "mo|%"))))
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
			(annotated code "|")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_001.md") (start 1 3) (end 1 4) (annotation error) (line-text "mo|%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 4) (end 1 5))
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
			(annotated code "%")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_001.md") (start 1 4) (end 1 5) (annotation error) (line-text "mo|%")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpBar,OpPercent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
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
