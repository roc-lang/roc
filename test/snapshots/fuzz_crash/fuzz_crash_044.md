# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{{0
}}

""
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_044.md:1:20:1:21
UNEXPECTED STATEMENT - fuzz_crash_044.md:1:21:1:22
UNEXPECTED STATEMENT - fuzz_crash_044.md:1:22:1:23
UNEXPECTED STATEMENT - fuzz_crash_044.md:2:1:2:2
UNEXPECTED STATEMENT - fuzz_crash_044.md:2:2:2:3
UNEXPECTED STATEMENT - fuzz_crash_044.md:4:1:4:2
UNEXPECTED STATEMENT - fuzz_crash_044.md:4:2:4:2
UNEXPECTED STATEMENT - fuzz_crash_044.md:4:2:4:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 20) (end 1 21))
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
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_044.md") (start 1 20) (end 1 21) (annotation error) (line-text "app[]{f:platform\"\"}{{0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 21) (end 1 22))
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
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_044.md") (start 1 21) (end 1 22) (annotation error) (line-text "app[]{f:platform\"\"}{{0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 22) (end 1 23))
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
			(annotated code "0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_044.md") (start 1 22) (end 1 23) (annotation error) (line-text "app[]{f:platform\"\"}{{0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 1) (end 2 2))
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
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_044.md") (start 2 1) (end 2 2) (annotation error) (line-text "}}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 2) (end 2 3))
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
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_044.md") (start 2 2) (end 2 3) (annotation error) (line-text "}}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 4 1) (end 4 2))
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
			(source-region (file "fuzz_crash_044.md") (start 4 1) (end 4 2) (annotation error) (line-text "\"\""))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 4 2) (end 4 2))
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
			(source-region (file "fuzz_crash_044.md") (start 4 2) (end 4 2) (annotation error) (line-text "\"\""))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 4 2) (end 4 3))
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
			(source-region (file "fuzz_crash_044.md") (start 4 2) (end 4 3) (annotation error) (line-text "\"\"")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,OpenCurly,Int,
CloseCurly,CloseCurly,
StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides)
		(record-field (name "f")
			(e-string
				(e-string-part (raw ""))))
		(packages
			(record-field (name "f")
				(e-string
					(e-string-part (raw ""))))))
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }



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
