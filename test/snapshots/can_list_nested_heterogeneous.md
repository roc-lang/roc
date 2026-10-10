# META
~~~ini
description=Heterogeneous nested list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [1], ["hello"]]
~~~
# EXPECTED
TYPE MISMATCH - can_list_nested_heterogeneous.md:1:12:1:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 12) (end 1 19))
		(headline
			(reflow "This string literal must have the same type as a number literal, and nothing in this program determines a type that can be both:"))
		(document
			(source-region (file "can_list_nested_heterogeneous.md") (start 1 12) (end 1 19) (annotation error) (line-text "[[], [1], [\"hello\"]]"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
OpenSquare,OpenSquare,CloseSquare,Comma,OpenSquare,Int,CloseSquare,Comma,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list)
	(e-list
		(e-int (raw "1")))
	(e-list
		(e-string
			(e-string-part (raw "hello")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-empty_list)
		(e-list
			(elems
				(e-runtime-error (tag "erroneous_value_expr"))))
		(e-list
			(elems
				(e-runtime-error (tag "erroneous_value_expr"))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Dec))"))
~~~
