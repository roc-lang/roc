# META
~~~ini
description=Deeply nested closures
type=repl
~~~
# SOURCE
~~~roc
» (((|a| |b| |c| a + b + c)(100))(20))(3)
~~~
# OUTPUT
Evaluation error: error.TypeMismatch
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-call @1.2-1.41
	(e-call @1.3-1.37
		(e-call @1.4-1.32
			(e-lambda @1.5-1.26
				(args
					(p-assign @1.6-1.7 (ident "a")))
				(e-closure @1.9-1.26
					(captures
						(capture @1.6-1.7 (ident "a")))
					(e-lambda @1.9-1.26
						(args
							(p-assign @1.10-1.11 (ident "b")))
						(e-closure @1.13-1.26
							(captures
								(capture @1.6-1.7 (ident "a"))
								(capture @1.10-1.11 (ident "b")))
							(e-lambda @1.13-1.26
								(args
									(p-assign @1.14-1.15 (ident "c")))
								(e-binop @1.17-1.26 (op "add")
									(e-binop @1.17-1.22 (op "add")
										(e-lookup-local @1.17-1.18
											(p-assign @1.6-1.7 (ident "a")))
										(e-lookup-local @1.21-1.22
											(p-assign @1.10-1.11 (ident "b"))))
									(e-lookup-local @1.25-1.26
										(p-assign @1.14-1.15 (ident "c")))))))))
			(e-int @1.28-1.31 (value "100")))
		(e-int @1.34-1.36 (value "20")))
	(e-int @1.39-1.40 (value "3")))
~~~
# TYPES
~~~clojure
(expr @1.2-1.41 (type "Num(_size)"))
~~~
