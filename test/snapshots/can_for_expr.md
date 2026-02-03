# META
~~~ini
description=Test for loop as expression
type=expr
~~~
# SOURCE
~~~roc
for x in [1, 2, 3] { dbg x }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,KwDbg,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-for
	(p-ident (raw "x"))
	(e-list
		(e-int (raw "1"))
		(e-int (raw "2"))
		(e-int (raw "3")))
	(e-block
		(statements
			(s-dbg
				(e-ident (raw "x"))))))
~~~
# FORMATTED
~~~roc
for x in [1, 2, 3] {
	dbg x
}
~~~
# CANONICALIZE
~~~clojure
(e-for
	(p-assign (ident "x"))
	(e-list
		(elems
			(e-num (value "1"))
			(e-num (value "2"))
			(e-num (value "3"))))
	(e-block
		(e-dbg
			(e-lookup-local
				(p-assign (ident "x"))))))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
