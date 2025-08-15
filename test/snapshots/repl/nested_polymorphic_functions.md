# META
~~~ini
description=Nested polymorphic function calls
type=repl
~~~
# SOURCE
~~~roc
» (|identity| { a: identity(10), b: identity(20), c: identity(30) })(|x| x)
~~~
# OUTPUT
<record>
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-call @1.2-1.75
	(e-lambda @1.3-1.67
		(args
			(p-assign @1.4-1.12 (ident "identity")))
		(e-record @1.14-1.67
			(fields
				(field (name "a")
					(e-call @1.19-1.31
						(e-lookup-local @1.19-1.27
							(p-assign @1.4-1.12 (ident "identity")))
						(e-int @1.28-1.30 (value "10"))))
				(field (name "b")
					(e-call @1.36-1.48
						(e-lookup-local @1.36-1.44
							(p-assign @1.4-1.12 (ident "identity")))
						(e-int @1.45-1.47 (value "20"))))
				(field (name "c")
					(e-call @1.53-1.65
						(e-lookup-local @1.53-1.61
							(p-assign @1.4-1.12 (ident "identity")))
						(e-int @1.62-1.64 (value "30")))))))
	(e-lambda @1.69-1.74
		(args
			(p-assign @1.70-1.71 (ident "x")))
		(e-lookup-local @1.73-1.74
			(p-assign @1.70-1.71 (ident "x")))))
~~~
# TYPES
~~~clojure
(expr @1.2-1.75 (type "{ a: Num(_size), b: _field, c: _field2 }"))
~~~
