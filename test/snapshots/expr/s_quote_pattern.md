# META
~~~ini
description=Single quote literal's in patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
	['#'] => ...
	['a'.U8, 'b'.U8] => ...
	_ => ...
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - s_quote_pattern.md:1:7:1:10
# PROBLEMS
── ● unconditional condition ──────────────────────────── s_quote_pattern.md:1:7

This match value is known at compile time, so this match will always inspect
the same value.

match ... {
      ^^^

# TOKENS
~~~zig
KwMatch,TripleDot,OpenCurly,
OpenSquare,SingleQuote,CloseSquare,OpFatArrow,TripleDot,
OpenSquare,SingleQuote,NoSpaceDotUpperIdent,Comma,SingleQuote,NoSpaceDotUpperIdent,CloseSquare,OpFatArrow,TripleDot,
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
				(p-single-quote (raw "'a'") (type "U8"))
				(p-single-quote (raw "'b'") (type "U8")))
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
