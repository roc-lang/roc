# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu:;::::::::::::::le[%
~~~
# EXPECTED
UNEXPECTED TYPE SYNTAX - fuzz_crash_002.md:1:6:1:7
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:7:1:9
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:9:1:11
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:11:1:13
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:13:1:15
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:15:1:17
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:17:1:19
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:19:1:21
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:21:1:23
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:23:1:24
UNEXPECTED STATEMENT - fuzz_crash_002.md:1:24:1:25
MALFORMED TYPE - fuzz_crash_002.md:1:6:1:7
DECLARATION HAS NO VALUE - fuzz_crash_002.md:1:1:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 1 6) (end 1 7))
		(headline
			(reflow "I was parsing a type annotation, and this token cannot start a type here."))
		(document
			(reflow "Types can be type variables, uppercase type names, function types, tuples, records, or tag unions.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code ";")
			(text " here.")
			(line-break)
			(reflow "This token is malformed, so it cannot be used as ordinary Roc syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 6) (end 1 7) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 7) (end 1 9))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 7) (end 1 9) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 9) (end 1 11))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 9) (end 1 11) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 11) (end 1 13))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 11) (end 1 13) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 13) (end 1 15))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 13) (end 1 15) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 15) (end 1 17))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 15) (end 1 17) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 17) (end 1 19))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 17) (end 1 19) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 19) (end 1 21))
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
			(annotated code "::")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 19) (end 1 21) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 21) (end 1 23))
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
			(annotated code "le")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 21) (end 1 23) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
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
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 23) (end 1 24) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
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
			(annotated code "%")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_002.md") (start 1 24) (end 1 25) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 1 6) (end 1 7))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "fuzz_crash_002.md") (start 1 6) (end 1 7) (annotation error) (line-text "modu:;::::::::::::::le[%"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 1) (end 1 7))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_002.md") (start 1 1) (end 1 7) (annotation error) (line-text "modu:;::::::::::::::le[%"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,MalformedUnknownToken,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,OpDoubleColon,LowerIdent,OpenSquare,OpPercent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "modu")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))
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
modu :
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "modu"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
