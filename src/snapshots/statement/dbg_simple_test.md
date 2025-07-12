# META
~~~ini
description=Simple debug test to understand parsing behavior
type=file
~~~
# SOURCE
~~~roc
module [test]

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),OpenCurly(3:8-3:9),
LowerIdent(4:5-4:6),OpAssign(4:7-4:8),Int(4:9-4:11),
KwDbg(5:5-5:8),NoSpaceOpenRound(5:8-5:9),LowerIdent(5:9-5:10),CloseRound(5:10-5:11),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(file @1.1-6.2
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13 (text "test"))))
	(statements
		(s-decl @3.1-6.2
			(p-ident @3.1-3.5 (raw "test"))
			(e-block @3.8-6.2
				(statements
					(s-decl @4.5-4.11
						(p-ident @4.5-4.6 (raw "x"))
						(e-int @4.9-4.11 (raw "42")))
					(s-dbg @5.5-5.11
						(e-tuple @5.8-5.11
							(e-ident @5.9-5.10 (raw "x")))))))))
~~~
# FORMATTED
~~~roc
module [test]

test = {
	x = 42
	dbg (x)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "test"))
		(e-block @3.8-6.2
			(s-let @4.5-4.11
				(p-assign @4.5-4.6 (ident "x"))
				(e-int @4.9-4.11 (value "42")))
			(e-dbg @5.5-5.11
				(e-lookup-local @5.9-5.10
					(p-assign @4.5-4.6 (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "*")))
	(expressions
		(expr @3.8-6.2 (type "*"))))
~~~
