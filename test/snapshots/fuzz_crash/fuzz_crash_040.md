# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
app[]{f:platform""}{
o:0)
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_040.md:1:20:1:21
UNEXPECTED TYPE SYNTAX - fuzz_crash_040.md:2:3:2:4
UNEXPECTED STATEMENT - fuzz_crash_040.md:2:4:2:5
MALFORMED TYPE - fuzz_crash_040.md:2:3:2:4
DECLARATION HAS NO VALUE - fuzz_crash_040.md:2:1:2:4
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
			(source-region (file "fuzz_crash_040.md") (start 1 20) (end 1 21) (annotation error) (line-text "app[]{f:platform\"\"}{"))))
	(report
		(severity runtime_error)
		(title "Unexpected Type Syntax")
		(region (start 2 3) (end 2 4))
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
			(annotated code "0")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_040.md") (start 2 3) (end 2 4) (annotation error) (line-text "o:0)"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 4) (end 2 5))
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
			(source-region (file "fuzz_crash_040.md") (start 2 4) (end 2 5) (annotation error) (line-text "o:0)"))))
	(report
		(severity runtime_error)
		(title "Malformed Type")
		(region (start 2 3) (end 2 4))
		(headline
			(reflow "This type annotation is malformed or contains invalid syntax."))
		(document
			(source-region (file "fuzz_crash_040.md") (start 2 3) (end 2 4) (annotation error) (line-text "o:0)"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 2 1) (end 2 4))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_040.md") (start 2 1) (end 2 4) (annotation error) (line-text "o:0)"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,OpenCurly,
LowerIdent,OpColon,Int,CloseRound,
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
		(s-type-anno (name "o")
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
app [] { f: platform "" }

o :
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "o"))
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
