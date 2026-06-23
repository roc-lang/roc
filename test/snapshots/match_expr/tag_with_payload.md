# META
~~~ini
description=Match expression with tag patterns containing payloads
type=expr
~~~
# SOURCE
~~~roc
match shape {
    Circle(radius) => 3.14 * radius * radius
    Rectangle(width, height) => width * height
    Triangle(base, height) => 0.5 * base * height
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,Float,OpStar,LowerIdent,OpStar,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,OpStar,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,Float,OpStar,LowerIdent,OpStar,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "shape"))
	(branches
		(branch
			(p-tag (raw "Circle")
				(p-ident (raw "radius")))
			(e-binop (op "*")
				(e-binop (op "*")
					(e-frac (raw "3.14"))
					(e-ident (raw "radius")))
				(e-ident (raw "radius"))))
		(branch
			(p-tag (raw "Rectangle")
				(p-ident (raw "width"))
				(p-ident (raw "height")))
			(e-binop (op "*")
				(e-ident (raw "width"))
				(e-ident (raw "height"))))
		(branch
			(p-tag (raw "Triangle")
				(p-ident (raw "base"))
				(p-ident (raw "height")))
			(e-binop (op "*")
				(e-binop (op "*")
					(e-frac (raw "0.5"))
					(e-ident (raw "base")))
				(e-ident (raw "height"))))))
~~~
# FORMATTED
~~~roc
match shape {
	Circle(radius) => 3.14 * radius * radius
	Rectangle(width, height) => width * height
	Triangle(base, height) => 0.5 * base * height
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
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 72)
						(receiver
							(e-dispatch-call (method "times") (constraint-fn-var 70)
								(receiver
									(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
								(args
									(e-lookup-local
										(p-assign (ident "radius"))))))
						(args
							(e-lookup-local
								(p-assign (ident "radius")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 75)
						(receiver
							(e-lookup-local
								(p-assign (ident "width"))))
						(args
							(e-lookup-local
								(p-assign (ident "height")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "times") (constraint-fn-var 113)
						(receiver
							(e-dispatch-call (method "times") (constraint-fn-var 111)
								(receiver
									(e-dec-small (numerator "5") (denominator-power-of-ten "1") (value "0.5")))
								(args
									(e-lookup-local
										(p-assign (ident "base"))))))
						(args
							(e-lookup-local
								(p-assign (ident "height"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
