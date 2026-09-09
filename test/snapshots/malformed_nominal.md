# META
~~~ini
description=Malformed nominal declaration name recovers with unexpected-statement diagnostics
type=file:malformed_nominal.roc
~~~
# SOURCE
~~~roc
@2 := {}

foo = "one"

bar = "two"
~~~
# EXPECTED
UNEXPECTED STATEMENT - malformed_nominal.md:1:1:1:3
UNEXPECTED STATEMENT - malformed_nominal.md:1:4:1:6
UNEXPECTED STATEMENT - malformed_nominal.md:1:7:1:8
UNEXPECTED STATEMENT - malformed_nominal.md:1:8:1:9
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
			(annotated code "@2")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "malformed_nominal.md") (start 1 1) (end 1 3) (annotation error) (line-text "@2 := {}"))))
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
			(annotated code ":=")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "malformed_nominal.md") (start 1 4) (end 1 6) (annotation error) (line-text "@2 := {}"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 7) (end 1 8))
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
			(source-region (file "malformed_nominal.md") (start 1 7) (end 1 8) (annotation error) (line-text "@2 := {}"))))
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
			(annotated code "}")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "malformed_nominal.md") (start 1 8) (end 1 9) (annotation error) (line-text "@2 := {}")))))
~~~
# TOKENS
~~~zig
OpaqueName,OpColonEqual,OpenCurly,CloseCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
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
		(s-decl
			(p-ident (raw "foo"))
			(e-string
				(e-string-part (raw "one"))))
		(s-decl
			(p-ident (raw "bar"))
			(e-string
				(e-string-part (raw "two"))))))
~~~
# FORMATTED
~~~roc


foo = "one"

bar = "two"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-string
			(e-literal (string "one"))))
	(d-let
		(p-assign (ident "bar"))
		(e-string
			(e-literal (string "two")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str"))
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))
		(expr (type "Str"))))
~~~
