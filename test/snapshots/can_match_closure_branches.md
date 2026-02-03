# META
~~~ini
description=Test match expression with closures in branches
type=expr
~~~
# SOURCE
~~~roc
|isSimple| match isSimple {
    True => |x| x + 1
    False => |x| 10 * x
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
UpperIdent,OpFatArrow,OpBar,LowerIdent,OpBar,Int,OpStar,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "isSimple")))
	(e-match
		(e-ident (raw "isSimple"))
		(branches
			(branch
				(p-tag (raw "True"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-int (raw "1")))))
			(branch
				(p-tag (raw "False"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-binop (op "*")
						(e-int (raw "10"))
						(e-ident (raw "x"))))))))
~~~
# FORMATTED
~~~roc
|isSimple| match isSimple {
	True => |x| x + 1
	False => |x| 10 * x
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "isSimple")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "isSimple"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-applied-tag)))
					(value
						(e-lambda
							(args
								(p-assign (ident "x")))
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "x")))
								(e-num (value "1"))))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-applied-tag)))
					(value
						(e-lambda
							(args
								(p-assign (ident "x")))
							(e-binop (op "mul")
								(e-num (value "10"))
								(e-lookup-local
									(p-assign (ident "x")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "[True, False, ..] -> (a -> a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, a.times : a, a -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
