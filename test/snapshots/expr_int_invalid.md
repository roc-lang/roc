# META
~~~ini
description=Invalid integer literal that exceeds i128 range
type=expr
~~~
# SOURCE
~~~roc
99999999999999999999999999999999999999999
~~~
# EXPECTED
INVALID NUMBER - expr_int_invalid.md:1:1:1:42
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 1 1) (end 1 42))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "expr_int_invalid.md") (start 1 1) (end 1 42) (annotation error) (line-text "99999999999999999999999999999999999999999"))
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
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "99999999999999999999999999999999999999999"))
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
