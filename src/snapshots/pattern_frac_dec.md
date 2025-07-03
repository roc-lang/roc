# META
~~~ini
description=Fractional pattern literals have Dec type
type=expr
~~~
# SOURCE
~~~roc
match 3.14 {
    3.14 => 3.14
    x => x
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch(1:1-1:6),Float(1:7-1:11),OpenCurly(1:12-1:13),Newline(1:1-1:1),
Float(2:5-2:9),OpFatArrow(2:10-2:12),Float(2:13-2:17),Newline(1:1-1:1),
LowerIdent(3:5-3:6),OpFatArrow(3:7-3:9),LowerIdent(3:10-3:11),Newline(1:1-1:1),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-match
	(e-frac @1.7-1.11 (raw "3.14"))
	(branches
		(branch @2.5-3.6
			(p-frac @2.5-2.9 (raw "3.14"))
			(e-frac @2.13-2.17 (raw "3.14")))
		(branch @3.5-4.2
			(p-ident @3.5-3.6 (raw "x"))
			(e-ident @3.10-3.11 (qaul "") (raw "x")))))
~~~
# FORMATTED
~~~roc
match 3.14 {
	3.14 => 3.14
	x => x
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-4.2
	(match @1.1-4.2
		(cond
			(e-dec-small @1.7-1.11 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
		(branches
			(branch
				(patterns
					(p-small-dec @2.5-2.9 (degenerate false)))
				(value
					(e-dec-small @2.13-2.17 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
			(branch
				(patterns
					(p-assign @3.5-3.6 (ident "x") (degenerate false)))
				(value
					(e-lookup-local @3.10-3.11
						(pattern @3.5-3.6)))))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "Dec"))
~~~
