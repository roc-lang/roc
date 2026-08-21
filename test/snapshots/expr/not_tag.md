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
MISSING METHOD - not_tag.md:1:1:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 1) (end 1 8))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "not")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "not_tag.md") (start 1 1) (end 1 8) (annotation error) (line-text "!(C(2))"))
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
			(text "[C(a), ..] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end))))
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
(e-runtime-error (tag "erroneous_value_expr"))
~~~
# TYPES
~~~clojure
(expr (type "[C(Dec), ..]"))
~~~
