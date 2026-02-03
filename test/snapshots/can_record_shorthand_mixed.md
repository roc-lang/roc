# META
~~~ini
description=Test record with mixed shorthand and explicit field syntax
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    y = 10
    { x, y, z: 20 }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpColon,Int,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "10")))
		(e-record
			(field (field "x"))
			(field (field "y"))
			(field (field "z")
				(e-int (raw "20"))))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	y = 10
	{ x, y, z: 20 }
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(s-let
		(p-assign (ident "y"))
		(e-num (value "10")))
	(e-record
		(fields
			(field (name "x")
				(e-lookup-local
					(p-assign (ident "x"))))
			(field (name "y")
				(e-lookup-local
					(p-assign (ident "y"))))
			(field (name "z")
				(e-num (value "20"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ x: a, y: b, z: c } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
~~~
