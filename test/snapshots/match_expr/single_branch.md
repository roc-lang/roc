# META
~~~ini
description=Match expression with single branch (simple variable pattern)
type=expr
~~~
# SOURCE
~~~roc
match value {
    x => x + 1
}
~~~
# EXPECTED
POLYMORPHIC VALUE - single_branch.md:1:1:3:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 1 1) (end 3 2))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "single_branch.md") (start 1 1) (end 3 2) (annotation error) (line-text "match value {\n    x => x + 1\n}"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "a where [a.plus : a, Dec -> a]")
			(line-break)
			(reflow "Add an annotation or use this value in a way that fixes its concrete type."))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
LowerIdent,OpFatArrow,LowerIdent,OpPlus,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "value"))
	(branches
		(branch
			(p-ident (raw "x"))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
match value {
	x => x + 1
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
						(p-assign (ident "x"))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 217)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-num (value "1")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, Dec -> a]"))
~~~
