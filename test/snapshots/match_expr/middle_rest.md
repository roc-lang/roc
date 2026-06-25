# META
~~~ini
description=Match expression with rest patterns in middle position
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, .., last] => first + last
    [a, b, .. as middle, x, y] => a + b + x + y  
    [single] => single
    [] => 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest)
				(p-ident (raw "last")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "last"))))
		(branch
			(p-list
				(p-ident (raw "a"))
				(p-ident (raw "b"))
				(p-list-rest (name "middle"))
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-ident (raw "b")))
					(e-ident (raw "x")))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-ident (raw "single")))
			(e-ident (raw "single")))
		(branch
			(p-list)
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .., last] => first + last
	[a, b, .. as middle, x, y] => a + b + x + y
	[single] => single
	[] => 0
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
								(p-assign (ident "first"))
								(p-assign (ident "last")))
							(rest-at (index 1)))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 44)
						(receiver
							(e-lookup-local
								(p-assign (ident "first"))))
						(args
							(e-lookup-local
								(p-assign (ident "last")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "a"))
								(p-assign (ident "b"))
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 2)
								(p-assign (ident "middle"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 52)
						(receiver
							(e-dispatch-call (method "plus") (constraint-fn-var 50)
								(receiver
									(e-dispatch-call (method "plus") (constraint-fn-var 48)
										(receiver
											(e-lookup-local
												(p-assign (ident "a"))))
										(args
											(e-lookup-local
												(p-assign (ident "b"))))))
								(args
									(e-lookup-local
										(p-assign (ident "x"))))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "single"))))))
				(value
					(e-lookup-local
						(p-assign (ident "single")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns))))
				(value
					(e-num (value "0")))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
