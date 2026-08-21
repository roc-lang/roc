# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
H{o,
    ]
foo =

    "on        (string 'onmo %')))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNCLOSED STRING - fuzz_crash_010.md:5:5:5:35
TYPE APPLICATION NEEDS PARENTHESES - fuzz_crash_010.md:1:2:1:3
UNEXPECTED STATEMENT - fuzz_crash_010.md:1:3:1:4
UNEXPECTED STATEMENT - fuzz_crash_010.md:1:4:1:5
UNEXPECTED STATEMENT - fuzz_crash_010.md:2:6:2:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "ASCII Control Character")
		(headline
			(reflow "ASCII control characters are not allowed in Roc source code."))
		(document))
	(report
		(severity runtime_error)
		(title "Unclosed String")
		(region (start 5 5) (end 5 35))
		(headline
			(reflow "This string is missing a closing quote."))
		(document
			(source-region (file "fuzz_crash_010.md") (start 5 5) (end 5 35) (annotation error) (line-text "    \"on        (string 'onmo %')))"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 1 2) (end 1 3))
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
			(annotated code "{")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_010.md") (start 1 2) (end 1 3) (annotation error) (line-text "H{o,"))))
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
			(annotated code "o")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_010.md") (start 1 3) (end 1 4) (annotation error) (line-text "H{o,"))))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_010.md") (start 1 4) (end 1 5) (annotation error) (line-text "H{o,"))))
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_010.md") (start 2 6) (end 2 7) (annotation error) (line-text "  \u{1f}  ]")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpenCurly,LowerIdent,Comma,
CloseSquare,
LowerIdent,OpAssign,
StringStart,StringPart,StringEnd,
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
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "on        (string 'onmo %')))"))))))
~~~
# FORMATTED
~~~roc


foo =

	"on        (string 'onmo %')))"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "on        (string 'onmo %')))")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~
