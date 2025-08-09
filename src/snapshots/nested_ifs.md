# META
~~~ini
description=Test evaluation of nested if expressions
type=repl
~~~
# SOURCE
~~~roc
» if 5 > 3 (if 1 > 2 3 else 4) else 5
~~~
# OUTPUT
4
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-if @1.1-1.36
	(if-branches
		(if-branch
			(e-binop @1.4-1.9 (op "gt")
				(e-int @1.4-1.5 (value "5"))
				(e-int @1.8-1.9 (value "3")))
			(e-if @1.11-1.28
				(if-branches
					(if-branch
						(e-binop @1.14-1.19 (op "gt")
							(e-int @1.14-1.15 (value "1"))
							(e-int @1.18-1.19 (value "2")))
						(e-int @1.20-1.21 (value "3"))))
				(if-else
					(e-int @1.27-1.28 (value "4"))))))
	(if-else
		(e-int @1.35-1.36 (value "5"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.36 (type "Num(_size)"))
~~~
