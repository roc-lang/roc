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
(e-call (constraint-fn-var 217)
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-call (constraint-fn-var 212)
			(e-lookup-associated-resolved (source "Bool.not") (builtin) (target-node "17379") (target-def "17379"))
			(e-lookup-local
				(p-assign (ident "x")))))
	(e-tag (name "True")))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
