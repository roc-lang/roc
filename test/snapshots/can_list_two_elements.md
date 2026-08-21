# META
~~~ini
description=List with exactly two incompatible elements
type=expr
~~~
# SOURCE
~~~roc
[1, "hello"]
~~~
# EXPECTED
TYPE MISMATCH - can_list_two_elements.md:1:5:1:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 5) (end 1 12))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "can_list_two_elements.md") (start 1 5) (end 1 12) (annotation error) (line-text "[1, \"hello\"]"))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
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
		(e-num (value "1"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Dec)"))
~~~
