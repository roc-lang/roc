# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
Fli/main.roc" }

Pair(a, b+ : (
~~~
# EXPECTED
UNCLOSED STRING - fuzz_crash_021.md:1:13:1:16
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_021.md:1:4:1:5
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:5:1:9
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:9:1:13
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:13:1:14
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:14:1:16
UNEXPECTED STATEMENT - fuzz_crash_021.md:1:16:1:16
EXPECTED TYPE SEPARATOR - fuzz_crash_021.md:3:1:3:5
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 1 13) (end 1 16))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_021.md") (start 1 13) (end 1 16) (annotation error) (line-text "Fli/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 1 4) (end 1 5))
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
			(annotated code "/")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_021.md") (start 1 4) (end 1 5) (annotation error) (line-text "Fli/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 5) (end 1 9))
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
			(annotated code "main")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_021.md") (start 1 5) (end 1 9) (annotation error) (line-text "Fli/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 9) (end 1 13))
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
			(annotated code ".roc")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_021.md") (start 1 9) (end 1 13) (annotation error) (line-text "Fli/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 13) (end 1 14))
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
			(source-region (file "fuzz_crash_021.md") (start 1 13) (end 1 14) (annotation error) (line-text "Fli/main.roc\" }"))))
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
			(annotated code " }")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_021.md") (start 1 14) (end 1 16) (annotation error) (line-text "Fli/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 16) (end 1 16))
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
			(source-region (file "fuzz_crash_021.md") (start 1 16) (end 1 16) (annotation error) (line-text "Fli/main.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Expected Type Separator")
		(region (start 3 1) (end 3 5))
		(headline
			(reflow "I was parsing type parameters, and I expected `,` or `)`."))
		(document
			(reflow "Separate type parameters with commas and close the parameter list with ")
			(annotated code ")")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Result(ok, err)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "Pair")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_021.md") (start 3 1) (end 3 5) (annotation error) (line-text "Pair(a, b+ : (")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpSlash,LowerIdent,NoSpaceDotLowerIdent,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,OpPlus,OpColon,OpenRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_ty_anno_close_round_or_comma"))))
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
