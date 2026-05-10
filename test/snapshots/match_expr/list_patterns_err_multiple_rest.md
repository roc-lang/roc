# META
~~~ini
description=Match expression with more than one rest pattern not permitted, should error
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,DoubleDot,Comma,LowerIdent,Comma,DoubleDot,CloseSquare,OpFatArrow,TripleDot,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "numbers"))
	(branches
		(branch
			(p-list
				(p-list-rest)
				(p-ident (raw "middle"))
				(p-list-rest))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
match numbers {
	[.., middle, ..] => ...
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
								(p-assign (ident "middle")))
							(rest-at (index 0)))))
				(value
					(e-not-implemented))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
