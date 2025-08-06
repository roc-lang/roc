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
(e-if @1.2-1.37
	(if-branches
		(if-branch
			(e-binop @1.5-1.10 (op "gt")
				(e-int @1.5-1.6 (value "5"))
				(e-int @1.9-1.10 (value "3")))
			(e-if @1.12-1.29
				(if-branches
					(if-branch
						(e-binop @1.15-1.20 (op "gt")
							(e-int @1.15-1.16 (value "1"))
							(e-int @1.19-1.20 (value "2")))
						(e-int @1.21-1.22 (value "3"))))
				(if-else
					(e-int @1.28-1.29 (value "4"))))))
	(if-else
		(e-int @1.36-1.37 (value "5"))))
~~~
