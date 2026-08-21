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
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "let_polymorphism_error.md") (start 1 11) (end 1 18) (annotation error) (line-text "[42, 4.2, \"hello\"]"))
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
		(e-num (value "42"))
		(e-dec-small (numerator "42") (denominator-power-of-ten "1") (value "4.2"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(expr (type "List(Dec)"))
~~~
