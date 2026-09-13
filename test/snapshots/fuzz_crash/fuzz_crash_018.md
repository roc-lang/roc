# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# EXPECTED
UNEXPECTED STATEMENT - fuzz_crash_018.md:1:1:1:2
UNEXPECTED STATEMENT - fuzz_crash_018.md:2:1:2:3
UNDECLARED TYPE - fuzz_crash_018.md:1:5:1:6
DECLARATION HAS NO VALUE - fuzz_crash_018.md:1:3:1:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 1 1) (end 1 2))
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
			(source-region (file "fuzz_crash_018.md") (start 1 1) (end 1 2) (annotation error) (line-text "0 b:S"))))
	(report
		(severity runtime_error)
		(title "Unexpected Statement")
		(region (start 2 1) (end 2 3))
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
			(annotated code ".R")
			(text " here.")
			(line-break)
			(reflow "Names that start with uppercase letters are used for tags, type names, and mod names in Roc.")
			(line-break)
			(line-break)
			(source-region (file "fuzz_crash_018.md") (start 2 1) (end 2 3) (annotation error) (line-text ".R"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 1 5) (end 1 6))
		(headline
			(reflow "The type ")
			(annotated code "S")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "fuzz_crash_018.md") (start 1 5) (end 1 6) (annotation error) (line-text "0 b:S"))))
	(report
		(severity warning)
		(title "Declaration Has No Value")
		(region (start 1 3) (end 1 6))
		(headline
			(reflow "This declaration has a type annotation but no implementation."))
		(document
			(source-region (file "fuzz_crash_018.md") (start 1 3) (end 1 6) (annotation error) (line-text "0 b:S"))
			(line-break)
			(line-break)
			(reflow "Add a value body here, or put hosted functions in a platform type mod so they are published through the host boundary."))))
~~~
# TOKENS
~~~zig
Int,LowerIdent,OpColon,UpperIdent,
DotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "b")
			(ty (name "S")))
		(s-malformed (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
b : S
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "b"))
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
