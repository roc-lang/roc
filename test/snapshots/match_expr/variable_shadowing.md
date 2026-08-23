# META
~~~ini
description=Match expression demonstrating variable shadowing between outer scope and branches
type=expr
~~~
# SOURCE
~~~roc
match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}
~~~
# EXPECTED
POLYMORPHIC VALUE - variable_shadowing.md:1:1:4:2
# PROBLEMS
── ✗ polymorphic value ─────────────────────────────── variable_shadowing.md:1:1

This top-level value still has an unresolved polymorphic type.

match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}

Its type is:
a where [a.plus : a, _arg -> a, a.times : a, Dec -> a]
Add an annotation or use this value in a way that fixes its concrete type.

# TOKENS
~~~zig
KwMatch,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpenCurly,
OpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenRound,UpperIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpStar,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-tuple
		(e-ident (raw "value"))
		(e-ident (raw "other")))
	(branches
		(branch
			(p-tuple
				(p-tag (raw "Some")
					(p-ident (raw "x")))
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-tuple
				(p-tag (raw "None"))
				(p-ident (raw "x")))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-int (raw "2"))))))
~~~
# FORMATTED
~~~roc
match (value, other) {
	(Some(x), y) => x + y
	(None, x) => x * 2
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tuple
				(elems
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-runtime-error (tag "ident_not_in_scope")))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-assign (ident "y"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 224)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-tuple
							(patterns
								(p-applied-tag)
								(p-assign (ident "x"))))))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 234)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-num (value "2")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, _arg -> a, a.times : a, Dec -> a]"))
~~~
