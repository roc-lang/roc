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
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-region (file "not_tag.md") (start 1 3) (end 1 7) (annotation error) (line-text "!(C(2))"))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[C(a), ..] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the function needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Bool")
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
(e-call
	(e-lookup-associated-resolved (source "Bool.not") (builtin) (target-node "17421") (target-def "17421"))
	(e-tag (name "C")
		(args
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
