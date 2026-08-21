# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1.U8, 2.U8, 300]
~~~
# EXPECTED
INVALID NUMBER - can_list_number_doesnt_fit.md:1:14:1:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 1 14) (end 1 17))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "can_list_number_doesnt_fit.md") (start 1 14) (end 1 17) (annotation error) (line-text "[1.U8, 2.U8, 300]"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U8")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenSquare,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-typed-int (raw "1") (type "U8"))
	(e-typed-int (raw "2") (type "U8"))
	(e-int (raw "300")))
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
		(e-typed-int (value "2") (type "U8"))
		(e-num (value "300"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
