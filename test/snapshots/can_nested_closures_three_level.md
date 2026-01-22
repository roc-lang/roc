# META
~~~ini
description=Test three-level nested closures with captured variables
type=expr
~~~
# SOURCE
~~~roc
|a|
    |b|
        |c| a + b + c
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,
OpBar,LowerIdent,OpBar,
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "a")))
	(e-lambda
		(args
			(p-ident (raw "b")))
		(e-lambda
			(args
				(p-ident (raw "c")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "b")))
				(e-ident (raw "c"))))))
~~~
# FORMATTED
~~~roc
|a|
	|b|
		|c| a + b + c
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "a")))
	(e-closure
		(captures
			(capture (ident "a")))
		(e-lambda
			(args
				(p-assign (ident "b")))
			(e-closure
				(captures
					(capture (ident "a"))
					(capture (ident "b")))
				(e-lambda
					(args
						(p-assign (ident "c")))
					(e-binop (op "add")
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "a")))
							(e-lookup-local
								(p-assign (ident "b"))))
						(e-lookup-local
							(p-assign (ident "c")))))))))
~~~
# TYPES
~~~clojure
(expr (type "d -> (e -> (e -> d)) where [d.plus : d, e -> d]"))
~~~
