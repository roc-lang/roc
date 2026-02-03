# META
~~~ini
description=Test match expression with alternatives
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    1 | 2 | 3 => "small"
    _ => "large"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,OpBar,Int,OpBar,Int,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-match
		(e-ident (raw "x"))
		(branches
			(branch
				(p-alternatives
					(p-int (raw "1"))
					(p-int (raw "2"))
					(p-int (raw "3")))
				(e-string
					(e-string-part (raw "small"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "large")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	1 | 2 | 3 => "small"
	_ => "large"
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "x"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-num (value "1")))
						(pattern (degenerate false)
							(p-num (value "2")))
						(pattern (degenerate false)
							(p-num (value "3"))))
					(value
						(e-string
							(e-literal (string "small")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-string
							(e-literal (string "large")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a -> Str where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
