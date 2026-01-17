# META
~~~ini
description=Test match expression
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    0 => "zero"
    1 => "one"
    _ => "other"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Int,OpFatArrow,StringStart,StringPart,StringEnd,
Int,OpFatArrow,StringStart,StringPart,StringEnd,
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
				(p-int (raw "0"))
				(e-string
					(e-string-part (raw "zero"))))
			(branch
				(p-int (raw "1"))
				(e-string
					(e-string-part (raw "one"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	0 => "zero"
	1 => "one"
	_ => "other"
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
							(p-num (value "0"))))
					(value
						(e-string
							(e-literal (string "zero")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-num (value "1"))))
					(value
						(e-string
							(e-literal (string "one")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-string
							(e-literal (string "other")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a -> Str where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
