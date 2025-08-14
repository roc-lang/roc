# META
~~~ini
description=Calculate Fibonacci number for 5
type=repl
~~~
# SOURCE
~~~roc
» fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
» fib(5)
~~~
# OUTPUT
assigned `fib`
---
5
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-block @1.1-1.1
	(s-let @1.1-1.1
		(p-assign @1.1-1.1 (ident "fib"))
		(e-closure @1.1-1.1
			(captures
				(capture @1.1-1.1 (ident "fib")))
			(e-lambda @1.1-1.1
				(args
					(p-assign @1.1-1.1 (ident "n")))
				(e-if @1.1-1.1
					(if-branches
						(if-branch
							(e-binop @1.1-1.1 (op "le")
								(e-lookup-local @1.1-1.1
									(p-assign @1.1-1.1 (ident "n")))
								(e-int @1.1-1.1 (value "1")))
							(e-lookup-local @1.1-1.1
								(p-assign @1.1-1.1 (ident "n")))))
					(if-else
						(e-binop @1.1-1.1 (op "add")
							(e-call @1.1-1.1
								(e-lookup-local @1.1-1.1
									(p-assign @1.1-1.1 (ident "fib")))
								(e-binop @1.1-1.1 (op "sub")
									(e-lookup-local @1.1-1.1
										(p-assign @1.1-1.1 (ident "n")))
									(e-int @1.1-1.1 (value "1"))))
							(e-call @1.1-1.1
								(e-lookup-local @1.1-1.1
									(p-assign @1.1-1.1 (ident "fib")))
								(e-binop @1.1-1.1 (op "sub")
									(e-lookup-local @1.1-1.1
										(p-assign @1.1-1.1 (ident "n")))
									(e-int @1.1-1.1 (value "2"))))))))))
	(e-call @1.1-1.1
		(e-lookup-local @1.1-1.1
			(p-assign @1.1-1.1 (ident "fib")))
		(e-int @1.1-1.1 (value "5"))))
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.5 (type "Error"))
~~~
