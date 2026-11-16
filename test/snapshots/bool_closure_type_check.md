# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBang,LowerIdent,CloseRound,NoSpaceOpenRound,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "x")))
			(unary "!"
				(e-ident (raw "x")))))
	(e-tag (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 216)
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-dispatch-call (method "not") (constraint-fn-var 213)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args)))
	(e-tag (name "True")))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
