# META
~~~ini
description=Test closure returned from match arm
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    Some(v) => |y| v + y
    None => |_| 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
UpperIdent,OpFatArrow,OpBar,Underscore,OpBar,Int,
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
				(p-tag (raw "Some")
					(p-ident (raw "v")))
				(e-lambda
					(args
						(p-ident (raw "y")))
					(e-binop (op "+")
						(e-ident (raw "v"))
						(e-ident (raw "y")))))
			(branch
				(p-tag (raw "None"))
				(e-lambda
					(args
						(p-underscore))
					(e-int (raw "0")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	Some(v) => |y| v + y
	None => |_| 0
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
							(p-applied-tag)))
					(value
						(e-closure
							(captures
								(capture (ident "v")))
							(e-lambda
								(args
									(p-assign (ident "y")))
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "v")))
									(e-lookup-local
										(p-assign (ident "y"))))))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-applied-tag)))
					(value
						(e-lambda
							(args
								(p-underscore))
							(e-num (value "0")))))))))
~~~
# TYPES
~~~clojure
(expr (type "[Some(a), None, .._others] -> (b -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a]"))
~~~
