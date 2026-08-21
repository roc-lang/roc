# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# EXPECTED
MISSING METHOD - bool_closure_type_check.md:1:6:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 6) (end 1 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "bool_closure_type_check.md") (start 1 6) (end 1 8) (annotation error) (line-text "(|x| !x)(True)"))
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
			(text "[True, ..]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBang,LowerIdent,CloseRound,NoSpaceOpenRound,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "x")))
			(unary "!"
				(e-ident (raw "x")))))
	(e-tag (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 209)
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-runtime-error (tag "erroneous_value_expr")))
	(e-tag (name "True")))
~~~
# TYPES
~~~clojure
(expr (type "[True, ..]"))
~~~
