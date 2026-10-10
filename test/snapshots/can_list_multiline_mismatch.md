# META
~~~ini
description=Multiline list with type mismatch
type=expr
~~~
# SOURCE
~~~roc
[
    42,
    "hello world",
    100
]
~~~
# EXPECTED
TYPE MISMATCH - can_list_multiline_mismatch.md:3:5:3:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 5) (end 3 18))
		(headline
			(reflow "This string literal must have the same type as a number literal, and nothing in this program determines a type that can be both:"))
		(document
			(source-region (file "can_list_multiline_mismatch.md") (start 3 5) (end 3 18) (annotation error) (line-text "    \"hello world\","))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
OpenSquare,
Int,Comma,
StringStart,StringPart,StringEnd,Comma,
Int,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "hello world")))
	(e-int (raw "100")))
~~~
# FORMATTED
~~~roc
[42, "hello world", 100]
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
