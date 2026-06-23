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
NIL
# PROBLEMS
NIL
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
					(e-dispatch-call (method "plus") (constraint-fn-var 48)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-num (value "1")))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
