# META
~~~ini
description=Headerless file with main function
type=file
~~~
# SOURCE
~~~roc
x = 5
main! = |_| x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Int(1:5-1:6),
LowerIdent(2:1-2:6),OpAssign(2:7-2:8),OpBar(2:9-2:10),Underscore(2:10-2:11),OpBar(2:11-2:12),LowerIdent(2:13-2:14),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.14
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.6
			(p-ident @1.1-1.2 (raw "x"))
			(e-int @1.5-1.6 (raw "5")))
		(s-decl @2.1-2.14
			(p-ident @2.1-2.6 (raw "main!"))
			(e-lambda @2.9-2.14
				(args
					(p-underscore))
				(e-ident @2.13-2.14 (raw "x"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "x"))
		(e-num @1.5-1.6 (value "5")))
	(d-let
		(p-assign @2.1-2.6 (ident "main!"))
		(e-closure @2.9-2.14
			(captures
				(capture @1.1-1.2 (ident "x")))
			(e-lambda @2.9-2.14
				(args
					(p-underscore @2.10-2.11))
				(e-lookup-local @2.13-2.14
					(p-assign @1.1-1.2 (ident "x")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Num(_size)"))
		(patt @2.1-2.6 (type "_arg -> Num(_size)")))
	(expressions
		(expr @1.5-1.6 (type "Num(_size)"))
		(expr @2.9-2.14 (type "_arg -> Num(_size)"))))
~~~
