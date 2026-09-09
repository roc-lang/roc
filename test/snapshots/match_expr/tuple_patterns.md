# META
~~~ini
description=Match expression with tuple destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match coord {
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
}
~~~
# EXPECTED
MISSING METHOD - tuple_patterns.md:2:21:2:29
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 2 21) (end 2 29))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_quote")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "tuple_patterns.md") (start 2 21) (end 2 29) (annotation error) (line-text "    (Zero, Zero) => \"origin\""))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_quote")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Zero, ..]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
OpenRound,LowerIdent,Comma,UpperIdent,CloseRound,OpFatArrow,LowerIdent,
OpenRound,UpperIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "coord"))
	(branches
		(branch
			(p-tuple
				(p-tag (raw "Zero"))
				(p-tag (raw "Zero")))
			(e-string
				(e-string-part (raw "origin"))))
		(branch
			(p-tuple
				(p-ident (raw "x"))
				(p-tag (raw "Zero")))
			(e-ident (raw "x")))
		(branch
			(p-tuple
				(p-tag (raw "Zero"))
				(p-ident (raw "y")))
			(e-ident (raw "y")))
		(branch
			(p-tuple
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
match coord {
	(Zero, Zero) => "origin"
	(x, Zero) => x
	(Zero, y) => y
	(x, y) => x
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-applied-tag)))))
				(value
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-assign (ident "x"))
								(p-applied-tag)))))
				(value
					(e-lookup-local
						(p-assign (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-assign (ident "y"))))))
				(value
					(e-lookup-local
						(p-assign (ident "y")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y"))))))
				(value
					(e-lookup-local
						(p-assign (ident "x"))))))))
~~~
# TYPES
~~~clojure
(expr (type "[Zero, ..]"))
~~~
