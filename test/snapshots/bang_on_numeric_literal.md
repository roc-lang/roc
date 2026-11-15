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
MISSING METHOD - bang_on_numeric_literal.md:1:2:1:3
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 2) (end 1 3))
		(headline
			(reflow "This number's type never resolved to a concrete type, so it has no")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "bang_on_numeric_literal.md") (start 1 2) (end 1 3) (annotation error) (line-text "!3"))
			(line-break)
			(reflow "Its type needs all of these:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a where [a.not : a -> a]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "None of the built-in number types")
			(reflow " ")
			(reflow "have a method named")
			(reflow " ")
			(annotated code "not")
			(reflow "."))))
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
(e-runtime-error (tag "erroneous_value_expr")
	(e-num (value "3")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
