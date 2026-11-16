# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C(2))
~~~
# EXPECTED
TYPE MISMATCH - not_tag.md:1:3:1:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 3) (end 1 7))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "!")
			(reflow " ")
			(reflow "operand is a tag union, so it must be a")
			(reflow " ")
			(annotated code "Bool")
			(reflow " ")
			(reflow "– either")
			(reflow " ")
			(annotated code "True")
			(reflow " ")
			(reflow "or")
			(reflow " ")
			(annotated code "False")
			(reflow "."))
		(document
			(source-region (file "not_tag.md") (start 1 3) (end 1 7) (annotation error) (line-text "!(C(2))"))
			(line-break)
			(reflow "It is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[C(a)] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But I need this to be a")
			(reflow " ")
			(annotated code "Bool")
			(reflow " ")
			(reflow "value."))))
~~~
# TOKENS
~~~zig
OpBang,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-tuple
		(e-apply
			(e-tag (raw "C"))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "not") (constraint-fn-var 211)
	(receiver
		(e-tag (name "C")
			(args
				(e-num (value "2")))))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
