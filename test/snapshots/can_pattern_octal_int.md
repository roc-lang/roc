# META
~~~ini
description=Test octal integer pattern matching
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    0o777 => "rwx"
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
				(p-int (raw "0o777"))
				(e-string
					(e-string-part (raw "rwx"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	0o777 => "rwx"
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
							(p-num (value "511"))))
					(value
						(e-string
							(e-literal (string "rwx")))))
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
