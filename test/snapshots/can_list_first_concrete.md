# META
~~~ini
description=Heterogeneous list where first element is concrete
type=expr
~~~
# SOURCE
~~~roc
[42, "world", 3.14]
~~~
# EXPECTED
TYPE MISMATCH - can_list_first_concrete.md:1:6:1:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 6) (end 1 13))
		(headline
			(reflow "This string literal must have the same type as a number literal, and nothing in this program determines a type that can be both:"))
		(document
			(source-region (file "can_list_first_concrete.md") (start 1 6) (end 1 13) (annotation error) (line-text "[42, \"world\", 3.14]"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,Float,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "world")))
	(e-frac (raw "3.14")))
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
