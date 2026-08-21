# META
~~~ini
description=Dec literal with negative exponent scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e-10
~~~
# EXPECTED
INVALID NUMBER - dec_scientific_negative_exp.md:1:1:1:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 1 1) (end 1 24))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "dec_scientific_negative_exp.md") (start 1 1) (end 1 24) (annotation error) (line-text "1.23456789012345678e-10"))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end))))
~~~
# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "1.23456789012345678e-10"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "erroneous_value_expr"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
