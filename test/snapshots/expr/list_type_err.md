# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, "hello"]
~~~
# EXPECTED
TYPE MISMATCH - list_type_err.md:1:8:1:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 8) (end 1 15))
		(headline
			(reflow "This string literal must have the same type as a number literal, and nothing in this program determines a type that can be both:"))
		(document
			(source-region (file "list_type_err.md") (start 1 8) (end 1 15) (annotation error) (line-text "[1, 2, \"hello\"]"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
OpenSquare,Int,Comma,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-int (raw "2"))
	(e-string
		(e-string-part (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Dec)"))
~~~
