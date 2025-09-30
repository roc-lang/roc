# META
~~~ini
description=Valid default app with main! function
type=file
~~~
# SOURCE
~~~roc
main! = |arg| {
    arg
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:6),OpAssign(1:7-1:8),OpBar(1:9-1:10),LowerIdent(1:10-1:13),OpBar(1:13-1:14),OpenCurly(1:15-1:16),
LowerIdent(2:5-2:8),
CloseCurly(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(type-module @1.1-1.6)
	(statements
		(s-decl @1.1-3.2
			(p-ident @1.1-1.6 (raw "main!"))
			(e-lambda @1.9-3.2
				(args
					(p-ident @1.10-1.13 (raw "arg")))
				(e-block @1.15-3.2
					(statements
						(e-ident @2.5-2.8 (raw "arg"))))))))
~~~
# FORMATTED
~~~roc
main! = |arg| {
	arg
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.6 (ident "main!"))
		(e-lambda @1.9-3.2
			(args
				(p-assign @1.10-1.13 (ident "arg")))
			(e-block @1.15-3.2
				(e-lookup-local @2.5-2.8
					(p-assign @1.10-1.13 (ident "arg")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.6 (type "_arg2 -> _ret")))
	(expressions
		(expr @1.9-3.2 (type "_arg2 -> _ret"))))
~~~
