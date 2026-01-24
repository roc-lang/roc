# META
~~~ini
description=Test float literals with scientific notation (very small and very large)
type=expr
~~~
# SOURCE
~~~roc
{
    tiny = 0.0000000001
    huge = 10000000000.0
    (tiny, huge)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Float,
LowerIdent,OpAssign,Float,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "tiny"))
			(e-frac (raw "0.0000000001")))
		(s-decl
			(p-ident (raw "huge"))
			(e-frac (raw "10000000000.0")))
		(e-tuple
			(e-ident (raw "tiny"))
			(e-ident (raw "huge")))))
~~~
# FORMATTED
~~~roc
{
	tiny = 0.0000000001
	huge = 10000000000.0
	(tiny, huge)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "tiny"))
		(e-dec-small (numerator "1") (denominator-power-of-ten "10") (value "0.0000000001")))
	(s-let
		(p-assign (ident "huge"))
		(e-frac-dec (value "10000000000")))
	(e-tuple
		(elems
			(e-lookup-local
				(p-assign (ident "tiny")))
			(e-lookup-local
				(p-assign (ident "huge"))))))
~~~
# TYPES
~~~clojure
(expr (type "(a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
