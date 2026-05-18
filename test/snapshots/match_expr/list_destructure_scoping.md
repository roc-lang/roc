# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "list"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first")))
			(e-ident (raw "first")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-ident (raw "second")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "second"))))))
~~~
# FORMATTED
~~~roc
match list {
	[first] => first
	[first, second] => first + second
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
						(p-list
							(patterns
								(p-assign (ident "first"))))))
				(value
					(e-lookup-local
						(p-assign (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "second"))))))
				(value
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "first")))
						(e-lookup-local
							(p-assign (ident "second")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, a -> a]"))
~~~
