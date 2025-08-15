# META
~~~ini
description=Deeply nested polymorphic function calls with multiple levels
type=repl
~~~
# SOURCE
~~~roc
» (|twice, identity| { a: twice(identity, 42), b: twice(|x| x + 1, 100) })(|f, val| f(f(val)), |x| x)
~~~
# OUTPUT
<record>
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-call @1.2-1.101
	(e-lambda @1.3-1.73
		(args
			(p-assign @1.4-1.9 (ident "twice"))
			(p-assign @1.11-1.19 (ident "identity")))
		(e-record @1.21-1.73
			(fields
				(field (name "a")
					(e-call @1.26-1.45
						(e-lookup-local @1.26-1.31
							(p-assign @1.4-1.9 (ident "twice")))
						(e-lookup-local @1.32-1.40
							(p-assign @1.11-1.19 (ident "identity")))
						(e-int @1.42-1.44 (value "42"))))
				(field (name "b")
					(e-call @1.50-1.71
						(e-lookup-local @1.50-1.55
							(p-assign @1.4-1.9 (ident "twice")))
						(e-lambda @1.56-1.65
							(args
								(p-assign @1.57-1.58 (ident "x")))
							(e-binop @1.60-1.65 (op "add")
								(e-lookup-local @1.60-1.61
									(p-assign @1.57-1.58 (ident "x")))
								(e-int @1.64-1.65 (value "1"))))
						(e-int @1.67-1.70 (value "100")))))))
	(e-lambda @1.75-1.93
		(args
			(p-assign @1.76-1.77 (ident "f"))
			(p-assign @1.79-1.82 (ident "val")))
		(e-call @1.84-1.93
			(e-lookup-local @1.84-1.85
				(p-assign @1.76-1.77 (ident "f")))
			(e-call @1.86-1.92
				(e-lookup-local @1.86-1.87
					(p-assign @1.76-1.77 (ident "f")))
				(e-lookup-local @1.88-1.91
					(p-assign @1.79-1.82 (ident "val"))))))
	(e-lambda @1.95-1.100
		(args
			(p-assign @1.96-1.97 (ident "x")))
		(e-lookup-local @1.99-1.100
			(p-assign @1.96-1.97 (ident "x")))))
~~~
# TYPES
~~~clojure
(expr @1.2-1.101 (type "{ a: _field, b: _field2 }"))
~~~
