# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}import	B	as
G	if 0{}else||0
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:3:2:5
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:6:2:7
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:7:2:8
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:8:2:9
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:9:2:13
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:13:2:14
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:14:2:15
UNEXPECTED STATEMENT - fuzz_crash_059.md:2:15:2:16
MOD NOT FOUND - fuzz_crash_059.md:1:20:2:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 3) (end 2 5))
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
			(annotated code "if")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_059.md") (start 2 3) (end 2 5) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 6) (end 2 7))
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
			(source-region (file "fuzz_crash_059.md") (start 2 6) (end 2 7) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 7) (end 2 8))
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
			(source-region (file "fuzz_crash_059.md") (start 2 7) (end 2 8) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 8) (end 2 9))
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
			(source-region (file "fuzz_crash_059.md") (start 2 8) (end 2 9) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 9) (end 2 13))
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
			(annotated code "else")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_059.md") (start 2 9) (end 2 13) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 13) (end 2 14))
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
			(source-region (file "fuzz_crash_059.md") (start 2 13) (end 2 14) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 14) (end 2 15))
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
			(source-region (file "fuzz_crash_059.md") (start 2 14) (end 2 15) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 15) (end 2 16))
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
			(source-region (file "fuzz_crash_059.md") (start 2 15) (end 2 16) (annotation error) (line-text "G\tif 0{}else||0"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 1 20) (end 2 2))
		(headline
			(text "The mod ")
			(annotated code "B")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "fuzz_crash_059.md") (start 1 20) (end 2 2) (annotation error) (line-text "app[]{f:platform\"\"}import\tB\tas\nG\tif 0{}else||0")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,KwImport,UpperIdent,KwAs,
UpperIdent,KwIf,Int,OpenCurly,CloseCurly,KwElse,OpBar,OpBar,Int,
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
		(s-import (raw "B") (alias "G"))
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
import B as G
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "B")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
