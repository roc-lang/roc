# META
~~~ini
description=Complex expressions with captures - lambda with conditionals and captures
type=expr
~~~
# SOURCE
~~~roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpGreaterThan,Int,OpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,KwElse,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-apply
		(e-tuple
			(e-lambda
				(args
					(p-ident (raw "outer")))
				(e-lambda
					(args
						(p-ident (raw "inner")))
					(e-if-then-else
						(e-binop (op ">")
							(e-ident (raw "outer"))
							(e-int (raw "0")))
						(e-tuple
							(e-binop (op "+")
								(e-ident (raw "outer"))
								(e-ident (raw "inner"))))
						(e-ident (raw "inner"))))))
		(e-int (raw "1")))
	(e-int (raw "-2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-call
		(e-lambda
			(args
				(p-assign (ident "outer")))
			(e-closure
				(captures
					(capture (ident "outer")))
				(e-lambda
					(args
						(p-assign (ident "inner")))
					(e-if
						(if-branches
							(if-branch
								(e-binop (op "gt")
									(e-lookup-local
										(p-assign (ident "outer")))
									(e-num (value "0")))
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "outer")))
									(e-lookup-local
										(p-assign (ident "inner"))))))
						(if-else
							(e-lookup-local
								(p-assign (ident "inner"))))))))
		(e-num (value "1")))
	(e-num (value "-2")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
