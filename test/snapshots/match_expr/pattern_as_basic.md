# META
~~~ini
description=Basic as pattern to bind both pattern and whole value
type=expr
~~~
# SOURCE
~~~roc
match (1, 2) {
    (x, y) as point => point
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - pattern_as_basic.md:1:7:1:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unconditional Condition")
		(region (start 1 7) (end 1 13))
		(headline
			(reflow "This")
			(reflow " ")
			(reflow "match value")
			(reflow " ")
			(reflow "is known at compile time, so")
			(reflow " ")
			(reflow "this match will always inspect the same value."))
		(document
			(source-region (file "pattern_as_basic.md") (start 1 7) (end 1 13) (annotation warning) (line-text "match (1, 2) {")))))
~~~
# TOKENS
~~~zig
KwMatch,OpenRound,Int,Comma,Int,CloseRound,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,KwAs,LowerIdent,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-tuple
		(e-int (raw "1"))
		(e-int (raw "2")))
	(branches
		(branch
			(p-as (name "point")
				(p-tuple
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-ident (raw "point")))))
~~~
# FORMATTED
~~~roc
match (1, 2) {
	(x, y) as point => point
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tuple
				(elems
					(e-num (value "1"))
					(e-num (value "2")))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-as (as "point")
							(p-tuple
								(patterns
									(p-assign (ident "x"))
									(p-assign (ident "y")))))))
				(value
					(e-lookup-local
						(p-as (as "point")
							(p-tuple
								(patterns
									(p-assign (ident "x"))
									(p-assign (ident "y")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "(Dec, Dec)"))
~~~
