# META
~~~ini
description=Debug statement in body context
type=file
~~~
# SOURCE
~~~roc
module [main]

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),OpenCurly(3:8-3:9),
LowerIdent(4:5-4:6),OpAssign(4:7-4:8),Int(4:9-4:11),
KwDbg(5:5-5:8),LowerIdent(5:9-5:10),
LowerIdent(6:5-6:6),OpPlus(6:7-6:8),Int(6:9-6:10),
CloseCurly(7:1-7:2),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.2
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "main"))))
	(statements
		(s-decl @3.1-7.2
			(p-ident @3.1-3.5 (raw "main"))
			(e-block @3.8-7.2
				(statements
					(s-decl @4.5-4.11
						(p-ident @4.5-4.6 (raw "x"))
						(e-int @4.9-4.11 (raw "42")))
					(s-dbg @5.5-5.10
						(e-ident @5.9-5.10 (raw "x")))
					(e-binop @6.5-6.10 (op "+")
						(e-ident @6.5-6.6 (raw "x"))
						(e-int @6.9-6.10 (raw "1"))))))))
~~~
# FORMATTED
~~~roc
module [main]

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
		(p-assign @3.1-3.5 (ident "main"))
		(e-block @3.8-7.2
			(s-let @4.5-4.11
				(p-assign @4.5-4.6 (ident "x"))
				(e-int @4.9-4.11 (value "42")))
			(s-dbg @5.5-5.10
				(e-lookup-local @5.9-5.10
					(p-assign @4.5-4.6 (ident "x"))))
			(e-binop @6.5-6.10 (op "add")
				(e-lookup-local @6.5-6.6
					(p-assign @4.5-4.6 (ident "x")))
				(e-int @6.9-6.10 (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Num(_size)")))
	(expressions
		(expr @3.8-7.2 (type "Num(_size)"))))
~~~
