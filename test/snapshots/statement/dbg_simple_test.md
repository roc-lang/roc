# META
~~~ini
description=Simple debug test to understand parsing behavior
type=snippet
~~~
# SOURCE
~~~roc
test = {
    x = 42
    dbg(x)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
KwDbg,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "test"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-int (raw "42")))
					(s-dbg
						(e-tuple
							(e-ident (raw "x")))))))))
~~~
# FORMATTED
~~~roc
test = {
	x = 42
	dbg (x)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test"))
		(e-block
			(s-let
				(p-assign (ident "x"))
				(e-num (value "42")))
			(e-dbg
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))))
~~~
