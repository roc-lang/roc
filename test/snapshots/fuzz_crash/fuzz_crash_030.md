# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
platform # Cd
	"foo" # Ce
	requires
		{	} #ose
			{n! : List(Str) => {}, # ure
			} #Ce
	exposes #rd
		[ .
		] # Cse
	packages # Cd
		{ # pen
pkg: 77"..c", mm} #
	provides # Cd
		[ # pen
ar,
		]
~~~
# EXPECTED
EXPECTED EXPOSED NAME - fuzz_crash_030.md:8:5:8:6
EXPECTED CLOSING BRACE - fuzz_crash_030.md:11:3:11:4
EXPECTED PROVIDES - fuzz_crash_030.md:12:9:12:12
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:12:12:13
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:13:12:14
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:15:12:17
UNEXPECTED STATEMENT - fuzz_crash_030.md:12:17:12:18
UNEXPECTED STATEMENT - fuzz_crash_030.md:13:2:13:10
UNEXPECTED STATEMENT - fuzz_crash_030.md:14:3:14:4
UNEXPECTED STATEMENT - fuzz_crash_030.md:15:1:15:3
UNEXPECTED STATEMENT - fuzz_crash_030.md:15:3:15:4
UNEXPECTED STATEMENT - fuzz_crash_030.md:16:3:16:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Exposed Name")
		(region (start 8 5) (end 8 6))
		(headline
			(reflow "I was parsing an exposing list, and I expected an exposed name."))
		(document
			(reflow "Exposing lists contain lowercase values, uppercase types or tags, and ")
			(annotated code "Type.*")
			(reflow " entries.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "package [main, Result, Result.*]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 8 5) (end 8 6) (annotation error) (line-text "\t\t[ ."))))
	(report
		(severity runtime_error)
		(title "Expected Closing Brace")
		(region (start 11 3) (end 11 4))
		(headline
			(reflow "I was parsing a `packages` record, and I expected a closing `}`."))
		(document
			(reflow "Close the packages record after the last package entry.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "packages { base: \"../base/main.roc\" }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 11 3) (end 11 4) (annotation error) (line-text "\t\t{ # pen"))))
	(report
		(severity runtime_error)
		(title "Expected Provides")
		(region (start 12 9) (end 12 12))
		(headline
			(reflow "I was parsing a platform header, and I expected the `provides` section."))
		(document
			(reflow "A platform header must map host symbols to Roc functions in a ")
			(annotated code "provides")
			(reflow " record.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "provides { \"roc_main\": main }")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "..c")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 12 9) (end 12 12) (annotation error) (line-text "pkg: 77\"..c\", mm} #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 12 12) (end 12 13))
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
			(source-region (file "fuzz_crash_030.md") (start 12 12) (end 12 13) (annotation error) (line-text "pkg: 77\"..c\", mm} #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 12 13) (end 12 14))
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
			(source-region (file "fuzz_crash_030.md") (start 12 13) (end 12 14) (annotation error) (line-text "pkg: 77\"..c\", mm} #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 12 15) (end 12 17))
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
			(annotated code "mm")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 12 15) (end 12 17) (annotation error) (line-text "pkg: 77\"..c\", mm} #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 12 17) (end 12 18))
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
			(source-region (file "fuzz_crash_030.md") (start 12 17) (end 12 18) (annotation error) (line-text "pkg: 77\"..c\", mm} #"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 13 2) (end 13 10))
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
			(annotated code "provides")
			(text " here.")
			(line-break)
			(reflow "That word is reserved by Roc, so it cannot be used as a name in this position.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 13 2) (end 13 10) (annotation error) (line-text "\tprovides # Cd"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 14 3) (end 14 4))
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
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 14 3) (end 14 4) (annotation error) (line-text "\t\t[ # pen"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 1) (end 15 3))
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
			(annotated code "ar")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 15 1) (end 15 3) (annotation error) (line-text "ar,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 15 3) (end 15 4))
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
			(source-region (file "fuzz_crash_030.md") (start 15 3) (end 15 4) (annotation error) (line-text "ar,"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 16 3) (end 16 4))
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_030.md") (start 16 3) (end 16 4) (annotation error) (line-text "\t\t]")))))
~~~
# TOKENS
~~~zig
KwPlatform,
StringStart,StringPart,StringEnd,
KwRequires,
OpenCurly,CloseCurly,
OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,OpenCurly,CloseCurly,Comma,
CloseCurly,
KwExposes,
OpenSquare,Dot,
CloseSquare,
KwPackages,
OpenCurly,
LowerIdent,OpColon,Int,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseCurly,
KwProvides,
OpenSquare,
LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_provides"))
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
#
# Cd
# pen

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
