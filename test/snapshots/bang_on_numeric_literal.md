# META
~~~ini
description=Bang operator on numeric literal should produce type error
type=expr
~~~
# SOURCE
~~~roc
!3
~~~
# EXPECTED
MISSING METHOD - bang_on_numeric_literal.md:1:1:1:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 1) (end 1 3))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "bang_on_numeric_literal.md") (start 1 1) (end 1 3) (annotation error) (line-text "!3"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "not")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This numeric literal was given the type")
			(reflow " ")
			(annotated code "Dec")
			(reflow " ")
			(reflow "because it was never used as any concrete number type. To use a different numeric type, add a suffix or a type annotation."))))
~~~
# TOKENS
~~~zig
OpBang,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-int (raw "3")))
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
(expr (type "Dec"))
~~~
