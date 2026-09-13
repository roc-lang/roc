# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}|(0,)|||0
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:20:1:21
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:21:1:22
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:22:1:23
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:23:1:24
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:24:1:25
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:25:1:26
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:26:1:27
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:27:1:28
UNEXPECTED STATEMENT - fuzz_crash_041.md:1:28:1:29
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
			(annotated code "|")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_041.md") (start 1 20) (end 1 21) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
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
			(annotated code "(")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_041.md") (start 1 21) (end 1 22) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
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
			(source-region (file "fuzz_crash_041.md") (start 1 22) (end 1 23) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 23) (end 1 24))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_041.md") (start 1 23) (end 1 24) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 24) (end 1 25))
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
			(source-region (file "fuzz_crash_041.md") (start 1 24) (end 1 25) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 25) (end 1 26))
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
			(source-region (file "fuzz_crash_041.md") (start 1 25) (end 1 26) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 26) (end 1 27))
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
			(source-region (file "fuzz_crash_041.md") (start 1 26) (end 1 27) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 27) (end 1 28))
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
			(source-region (file "fuzz_crash_041.md") (start 1 27) (end 1 28) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 28) (end 1 29))
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
			(source-region (file "fuzz_crash_041.md") (start 1 28) (end 1 29) (annotation error) (line-text "app[]{f:platform\"\"}|(0,)|||0")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpBar,NoSpaceOpenRound,Int,Comma,CloseRound,OpBar,OpBar,OpBar,Int,
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
