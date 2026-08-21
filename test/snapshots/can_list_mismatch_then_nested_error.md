# META
~~~ini
description=List with type mismatch followed by nested heterogeneous list
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", [3, "world"]]
~~~
# EXPECTED
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:2:1:3
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:5:1:12
TYPE MISMATCH - can_list_mismatch_then_nested_error.md:1:18:1:25
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 2) (end 1 3))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "can_list_mismatch_then_nested_error.md") (start 1 2) (end 1 3) (annotation error) (line-text "[1, \"hello\", [3, \"world\"]]"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "can_list_mismatch_then_nested_error.md") (start 1 14) (end 1 26) (annotation error) (line-text "[1, \"hello\", [3, \"world\"]]"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(a)")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 5) (end 1 12))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "can_list_mismatch_then_nested_error.md") (start 1 5) (end 1 12) (annotation error) (line-text "[1, \"hello\", [3, \"world\"]]"))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(a)")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 18) (end 1 25))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "can_list_mismatch_then_nested_error.md") (start 1 18) (end 1 25) (annotation error) (line-text "[1, \"hello\", [3, \"world\"]]"))
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
OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,Comma,OpenSquare,Int,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
	(e-list
		(e-int (raw "3"))
		(e-string
			(e-string-part (raw "world")))))
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
		(e-list
			(elems
				(e-num (value "3"))
				(e-runtime-error (tag "erroneous_value_expr"))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Dec))"))
~~~
