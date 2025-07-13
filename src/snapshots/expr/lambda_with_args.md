# META
~~~ini
description=Lambda with multiple arguments
type=expr
~~~
# SOURCE
~~~roc
|x, y| x + y
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar(1:1-1:2),LowerIdent(1:2-1:3),Comma(1:3-1:4),LowerIdent(1:5-1:6),OpBar(1:6-1:7),LowerIdent(1:8-1:9),OpPlus(1:10-1:11),LowerIdent(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-1.13
	(args
		(p-ident @1.2-1.3 (raw "x"))
		(p-ident @1.5-1.6 (raw "y")))
	(e-binop @1.8-1.13 (op "+")
		(e-ident @1.8-1.9 (raw "x"))
		(e-ident @1.12-1.13 (raw "y"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-1.13
	(args
		(p-assign @1.2-1.3 (ident "x"))
		(p-assign @1.5-1.6 (ident "y")))
	(e-binop @1.8-1.13 (op "add")
		(e-lookup-local @1.8-1.9
			(p-assign @1.2-1.3 (ident "x")))
		(e-lookup-local @1.12-1.13
			(p-assign @1.5-1.6 (ident "y")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "arg, arg2 -> a"))
~~~
