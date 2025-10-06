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
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),OpenCurly(1:8-1:9),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:11),
KwDbg(3:5-3:8),NoSpaceOpenRound(3:8-3:9),LowerIdent(3:9-3:10),CloseRound(3:10-3:11),
CloseCurly(4:1-4:2),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.2
	(type-module @1.1-1.5)
	(statements
		(s-decl @1.1-4.2
			(p-ident @1.1-1.5 (raw "test"))
			(e-block @1.8-4.2
				(statements
					(s-decl @2.5-2.11
						(p-ident @2.5-2.6 (raw "x"))
						(e-int @2.9-2.11 (raw "42")))
					(s-dbg @3.5-3.11
						(e-tuple @3.8-3.11
							(e-ident @3.9-3.10 (raw "x")))))))))
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
		(p-assign @1.1-1.5 (ident "test"))
		(e-block @1.8-4.2
			(s-let @2.5-2.11
				(p-assign @2.5-2.6 (ident "x"))
				(e-num @2.9-2.11 (value "42")))
			(e-dbg @3.5-3.11
				(e-lookup-local @3.9-3.10
					(p-assign @2.5-2.6 (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.5 (type "Num(_size)")))
	(expressions
		(expr @1.8-4.2 (type "Num(_size)"))))
~~~
