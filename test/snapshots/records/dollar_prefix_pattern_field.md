# META
~~~ini
description=A dollar-prefixed punned record pattern is an immutable binding
type=expr
~~~
# SOURCE
~~~roc
|{ $name }| $name
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,OpenCurly,LowerIdent,CloseCurly,OpBar,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-record
			(field (name "$name") (rest false))))
	(e-ident (raw "$name")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-record-destructure
			(destructs
				(record-destruct (label "$name") (ident "$name")
					(required
						(p-assign (ident "$name")))))))
	(e-lookup-local
		(p-assign (ident "$name"))))
~~~
# TYPES
~~~clojure
(expr (type "{ $name: a } -> a"))
~~~
