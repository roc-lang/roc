# META
~~~ini
description=Debug statement in body context
type=snippet
~~~
# SOURCE
~~~roc
main = {
    x = 42
    dbg x
    x + 1
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
KwDbg,LowerIdent,
LowerIdent,OpPlus,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-int (raw "42")))
					(s-dbg
						(e-ident (raw "x")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-int (raw "1"))))))))
~~~
# FORMATTED
~~~roc
main = {
	x = 42
	dbg x
	x + 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "x"))
				(e-num (value "42")))
			(s-dbg
				(e-lookup-local
					(p-assign (ident "x"))))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))))
~~~
