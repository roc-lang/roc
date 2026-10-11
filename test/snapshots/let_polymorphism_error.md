# META
~~~ini
description=Let-polymorphism error case - incompatible list elements
type=expr
~~~
# SOURCE
~~~roc
[42, 4.2, "hello"]
~~~
# EXPECTED
TYPE MISMATCH - let_polymorphism_error.md:1:11:1:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 11) (end 1 18))
		(headline
			(reflow "This string literal must have the same type as a number literal, and nothing in this program determines a type that can be both:"))
		(document
			(source-region (file "let_polymorphism_error.md") (start 1 11) (end 1 18) (annotation error) (line-text "[42, 4.2, \"hello\"]"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
OpenSquare,Int,Comma,Float,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-frac (raw "4.2"))
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
