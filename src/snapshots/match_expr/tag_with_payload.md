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
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `shape` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwMatch(1:1-1:6),LowerIdent(1:7-1:12),OpenCurly(1:13-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:11),NoSpaceOpenRound(2:11-2:12),LowerIdent(2:12-2:18),CloseRound(2:18-2:19),OpFatArrow(2:20-2:22),Float(2:23-2:27),OpStar(2:28-2:29),LowerIdent(2:30-2:36),OpStar(2:37-2:38),LowerIdent(2:39-2:45),Newline(1:1-1:1),
UpperIdent(3:5-3:14),NoSpaceOpenRound(3:14-3:15),LowerIdent(3:15-3:20),Comma(3:20-3:21),LowerIdent(3:22-3:28),CloseRound(3:28-3:29),OpFatArrow(3:30-3:32),LowerIdent(3:33-3:38),OpStar(3:39-3:40),LowerIdent(3:41-3:47),Newline(1:1-1:1),
UpperIdent(4:5-4:13),NoSpaceOpenRound(4:13-4:14),LowerIdent(4:14-4:18),Comma(4:18-4:19),LowerIdent(4:20-4:26),CloseRound(4:26-4:27),OpFatArrow(4:28-4:30),Float(4:31-4:34),OpStar(4:35-4:36),LowerIdent(4:37-4:41),OpStar(4:42-4:43),LowerIdent(4:44-4:50),Newline(1:1-1:1),
CloseCurly(5:1-5:2),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(e-match
	(e-ident @1.7-1.12 (raw "shape"))
	(branches
		(branch @2.5-3.14
			(p-tag @2.5-2.19 (raw "Circle")
				(p-ident @2.12-2.18 (raw "radius")))
			(e-binop @2.23-3.14 (op "*")
				(e-frac @2.23-2.27 (raw "3.14"))
				(e-binop @2.30-3.14 (op "*")
					(e-ident @2.30-2.36 (raw "radius"))
					(e-ident @2.39-2.45 (raw "radius")))))
		(branch @3.5-4.13
			(p-tag @3.5-3.29 (raw "Rectangle")
				(p-ident @3.15-3.20 (raw "width"))
				(p-ident @3.22-3.28 (raw "height")))
			(e-binop @3.33-4.13 (op "*")
				(e-ident @3.33-3.38 (raw "width"))
				(e-ident @3.41-3.47 (raw "height"))))
		(branch @4.5-5.2
			(p-tag @4.5-4.27 (raw "Triangle")
				(p-ident @4.14-4.18 (raw "base"))
				(p-ident @4.20-4.26 (raw "height")))
			(e-binop @4.31-5.2 (op "*")
				(e-frac @4.31-4.34 (raw "0.5"))
				(e-binop @4.37-5.2 (op "*")
					(e-ident @4.37-4.41 (raw "base"))
					(e-ident @4.44-4.50 (raw "height")))))))
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
(e-match @1.1-5.2
	(match @1.1-5.2
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(p-applied-tag @2.5-2.19 (degenerate false)))
				(value
					(e-binop @2.23-3.14 (op "mul")
						(e-dec-small @2.23-2.27 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
						(e-binop @2.30-3.14 (op "mul")
							(e-lookup-local @2.30-2.36
								(pattern @2.12-2.18))
							(e-lookup-local @2.39-2.45
								(pattern @2.12-2.18))))))
			(branch
				(patterns
					(p-applied-tag @3.5-3.29 (degenerate false)))
				(value
					(e-binop @3.33-4.13 (op "mul")
						(e-lookup-local @3.33-3.38
							(pattern @3.15-3.20))
						(e-lookup-local @3.41-3.47
							(pattern @3.22-3.28)))))
			(branch
				(patterns
					(p-applied-tag @4.5-4.27 (degenerate false)))
				(value
					(e-binop @4.31-5.2 (op "mul")
						(e-dec-small @4.31-4.34 (numerator "5") (denominator-power-of-ten "1") (value "0.5"))
						(e-binop @4.37-5.2 (op "mul")
							(e-lookup-local @4.37-4.41
								(pattern @4.14-4.18))
							(e-lookup-local @4.44-4.50
								(pattern @4.20-4.26)))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
