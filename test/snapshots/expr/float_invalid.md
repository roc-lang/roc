# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# EXPECTED
INVALID TUPLE ACCESS - float_invalid.md:1:1:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Tuple Access")
		(region (start 1 1) (end 1 8))
		(headline
			(reflow "This value is not a tuple, so it has no .15 element."))
		(document
			(source-region (file "float_invalid.md") (start 1 1) (end 1 8) (annotation error) (line-text "3.14.15")))))
~~~
# TOKENS
~~~zig
Float,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-frac (raw "3.14"))
	".15")
~~~
# FORMATTED
~~~roc
(3.14).15
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "erroneous_value_expr"))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
