# META
~~~ini
description=Crash is valid as a match branch expression
type=expr
~~~
# SOURCE
~~~roc
|result| match result {
	Ok(value) => value
	Err(message) => crash message
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,KwCrash,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "result")))
	(e-match
		(e-ident (raw "result"))
		(branches
			(branch
				(p-tag (raw "Ok")
					(p-ident (raw "value")))
				(e-ident (raw "value")))
			(branch
				(p-tag (raw "Err")
					(p-ident (raw "message")))
				(e-crash
					(e-ident (raw "message")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "result")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "result"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-applied-tag)))
					(value
						(e-lookup-local
							(p-assign (ident "value")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-applied-tag)))
					(value
						(e-run-low-level (op "crash")
							(args
								(e-lookup-local
									(p-assign (ident "message")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "[Err(Str), Ok(a)] -> a"))
~~~
