# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# EXPECTED
POLYMORPHIC VALUE - branch_scoping.md:1:1:6:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 1 1) (end 6 2))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "branch_scoping.md") (start 1 1) (end 6 2) (annotation error) (line-text "match result {\n    Ok(value) => value + 1\n    Err(value) => value - 1\n    Ok(different) => different * 2\n    Err(different) => different / 2\n}"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "a\n  where [\n    a.div_by : a, Dec -> a,\n    a.minus : a, Dec -> a,\n    a.plus : a, Dec -> a,\n    a.times : a, Dec -> a,\n  ]")
			(line-break)
			(reflow "Add an annotation or use this value in a way that fixes its concrete type."))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpBinaryMinus,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpSlash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "result"))
	(branches
		(branch
			(p-tag (raw "Ok")
				(p-ident (raw "value")))
			(e-binop (op "+")
				(e-ident (raw "value"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "value")))
			(e-binop (op "-")
				(e-ident (raw "value"))
				(e-int (raw "1"))))
		(branch
			(p-tag (raw "Ok")
				(p-ident (raw "different")))
			(e-binop (op "*")
				(e-ident (raw "different"))
				(e-int (raw "2"))))
		(branch
			(p-tag (raw "Err")
				(p-ident (raw "different")))
			(e-binop (op "/")
				(e-ident (raw "different"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match result {
	Ok(value) => value + 1
	Err(value) => value - 1
	Ok(different) => different * 2
	Err(different) => different / 2
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
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 240)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-num (value "1"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "minus") (constraint-fn-var 253)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-num (value "1"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 264)
						(receiver
							(e-lookup-local
								(p-assign (ident "different"))))
						(args
							(e-num (value "2"))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "div_by") (constraint-fn-var 275)
						(receiver
							(e-lookup-local
								(p-assign (ident "different"))))
						(args
							(e-num (value "2")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.div_by : a, Dec -> a, a.minus : a, Dec -> a, a.plus : a, Dec -> a, a.times : a, Dec -> a]"))
~~~
