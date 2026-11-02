# META
~~~ini
description=Calling a float literal as a function should produce an error, not crash
type=expr
~~~
# SOURCE
~~~roc
0.0()
~~~
# EXPECTED
TYPE MISMATCH - call_float_literal.md:1:1:1:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 1) (end 1 4))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "call_float_literal.md") (start 1 1) (end 1 4) (annotation error) (line-text "0.0()"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "call_float_literal.md") (start 1 1) (end 1 6) (annotation error) (line-text "0.0()"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "() -> _ret")
			(annotation-end))))
~~~
# TOKENS
~~~zig
Float,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-frac (raw "0.0")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 218)
	(e-runtime-error (tag "erroneous_value_expr")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
