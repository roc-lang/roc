# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_007.md:1:1:1:4
UNEXPECTED STATEMENT - fuzz_crash_007.md:1:4:1:6
UNEXPECTED STATEMENT - fuzz_crash_007.md:1:6:1:8
# PROBLEMS
~~~clojure
(reports
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
			(annotated code "ff8")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_007.md") (start 1 1) (end 1 4) (annotation error) (line-text "ff8.8.d"))))
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
			(annotated code ".8")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_007.md") (start 1 4) (end 1 6) (annotation error) (line-text "ff8.8.d"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 6) (end 1 8))
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
			(annotated code ".d")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_007.md") (start 1 6) (end 1 8) (annotation error) (line-text "ff8.8.d")))))
~~~
# TOKENS
~~~zig
LowerIdent,NoSpaceDotInt,NoSpaceDotLowerIdent,
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
