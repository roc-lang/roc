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
TYPE MISMATCH - bang_on_numeric_literal.md:1:2:1:3
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
			(source-region (file "bang_on_numeric_literal.md") (start 1 2) (end 1 3) (annotation error) (line-text "!3"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Bool")
			(annotation-end))))
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
(e-call (constraint-fn-var 215)
	(e-lookup-associated-resolved (source "Bool.not") (builtin) (target-node "17421") (target-def "17421"))
	(e-runtime-error (tag "erroneous_value_expr")))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
