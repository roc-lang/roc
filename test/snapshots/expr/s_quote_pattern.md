# META
~~~ini
description=Single quote literal's in patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
	['#'] => ...
	['a', 'b'] => ...
	_ => ...
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,TripleDot,OpenCurly,
OpenSquare,SingleQuote,CloseSquare,OpFatArrow,TripleDot,
OpenSquare,SingleQuote,Comma,SingleQuote,CloseSquare,OpFatArrow,TripleDot,
Underscore,OpFatArrow,TripleDot,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch
			(p-list
				(p-single-quote (raw "'#'")))
			(e-ellipsis))
		(branch
			(p-list
				(p-single-quote (raw "'a'"))
				(p-single-quote (raw "'b'")))
			(e-ellipsis))
		(branch
			(p-underscore)
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-not-implemented))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-num (value "35"))))))
				(value
					(e-not-implemented)))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-num (value "97"))
								(p-num (value "98"))))))
				(value
					(e-not-implemented)))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore)))
				(value
					(e-not-implemented))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
