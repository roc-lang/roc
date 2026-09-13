# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
import json.Json [foo, BAR]
~~~
# EXPECTED
UNEXPECTED STATEMENT - stmt_import.md:1:18:1:19
UNEXPECTED STATEMENT - stmt_import.md:1:19:1:22
UNEXPECTED STATEMENT - stmt_import.md:1:22:1:23
TYPE APPLICATION NEEDS PARENTHESES - stmt_import.md:1:27:1:28
DUPLICATE DEFINITION - stmt_import.md:1:1:1:17
# PROBLEMS
~~~clojure
(reports
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
			(annotated code "[")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "stmt_import.md") (start 1 18) (end 1 19) (annotation error) (line-text "import json.Json [foo, BAR]"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 19) (end 1 22))
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
			(annotated code "foo")
			(text " here.")
			(line-break)
			(reflow "Names that start with lowercase letters are value names or record field names, depending on the surrounding syntax.")
			(line-break)
			(line-break)
			(source-region (file "stmt_import.md") (start 1 19) (end 1 22) (annotation error) (line-text "import json.Json [foo, BAR]"))))
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
			(annotated code ",")
			(text " here.")
			(line-break)
			(reflow "A comma separates items, but there must be a valid item on both sides of it.")
			(line-break)
			(line-break)
			(source-region (file "stmt_import.md") (start 1 22) (end 1 23) (annotation error) (line-text "import json.Json [foo, BAR]"))))
	(report
		(severity runtime_error)
		(title "Type Application Needs Parentheses")
		(region (start 1 27) (end 1 28))
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
			(annotated code "]")
			(text " here.")
			(line-break)
			(reflow "This closes the current construct, so the parser was looking for the missing item before it.")
			(line-break)
			(line-break)
			(source-region (file "stmt_import.md") (start 1 27) (end 1 28) (annotation error) (line-text "import json.Json [foo, BAR]"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 1 1) (end 1 17))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "stmt_import.md") (start 1 1) (end 1 17) (annotation error) (line-text "import json.Json [foo, BAR]"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "stmt_import.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "stmt_import.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json [foo, BAR]")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,OpenSquare,LowerIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Json"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
import json.Json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import (mod "json.Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
