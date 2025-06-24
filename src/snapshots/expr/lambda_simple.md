# META
~~~ini
description=Lambda expression
type=expr
~~~
# SOURCE
~~~roc
|x| x + 1
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar(1:1-1:2),LowerIdent(1:2-1:3),OpBar(1:3-1:4),LowerIdent(1:5-1:6),OpPlus(1:7-1:8),Int(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-lambda @1-1-1-10
	(args
		(p-ident @1-2-1-3 (raw "x")))
	(e-binop @1-5-1-10 (op "+")
		(e-ident @1-5-1-6 (qaul "") (raw "x"))
		(e-int @1-9-1-10 (raw "1"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1-1-1-10 (id 78)
	(args
		(p-assign @1-2-1-3 (ident "x") (id 72)))
	(e-binop @1-5-1-10 (op "add")
		(e-lookup-local @1-5-1-6
			(pattern (id 72)))
		(e-int @1-9-1-10 (int-var 75) (precision-var 74) (literal "1") (value "TODO") (bound "u8"))))
~~~
# TYPES
~~~clojure
(expr (id 78) (type "*"))
~~~