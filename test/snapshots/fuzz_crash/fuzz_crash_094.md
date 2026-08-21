# META
~~~ini
description=Issue #10094: Invalid formatting for package exposing syntax
type=file
~~~
# SOURCE
~~~roc
dapkage[e,E.a.*]{}
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:1:1:8
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:8:1:9
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:9:1:10
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:10:1:11
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_094.md:1:12:1:14
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:14:1:16
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:16:1:17
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:17:1:18
UNEXPECTED STATEMENT - fuzz_crash_094.md:1:18:1:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 8))
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
			(annotated code "dapkage")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_094.md") (start 1 1) (end 1 8) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 8) (end 1 9))
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
			(source-region (file "fuzz_crash_094.md") (start 1 8) (end 1 9) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 9) (end 1 10))
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
			(annotated code "e")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_094.md") (start 1 9) (end 1 10) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 10) (end 1 11))
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
			(source-region (file "fuzz_crash_094.md") (start 1 10) (end 1 11) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 1 12) (end 1 14))
		(headline
			(reflow "I was parsing a type annotation, and I found a type argument without parentheses."))
		(document
			(reflow "Roc type applications use parentheses around their arguments. Write ")
			(annotated code "List(U8)")
			(reflow ", not ")
			(annotated code "List U8")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ".a")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_094.md") (start 1 12) (end 1 14) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 14) (end 1 16))
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
			(annotated code ".*")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_094.md") (start 1 14) (end 1 16) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 16) (end 1 17))
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
			(source-region (file "fuzz_crash_094.md") (start 1 16) (end 1 17) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 17) (end 1 18))
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
			(source-region (file "fuzz_crash_094.md") (start 1 17) (end 1 18) (annotation error) (line-text "dapkage[e,E.a.*]{}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 18) (end 1 19))
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
			(source-region (file "fuzz_crash_094.md") (start 1 18) (end 1 19) (annotation error) (line-text "dapkage[e,E.a.*]{}")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpenSquare,LowerIdent,Comma,UpperIdent,NoSpaceDotLowerIdent,DotStar,CloseSquare,OpenCurly,CloseCurly,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))
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
