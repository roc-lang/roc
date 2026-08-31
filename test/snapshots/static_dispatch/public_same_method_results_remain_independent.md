# META
~~~ini
description=Keeps same-name requirements separate when their results are independently visible in the inferred type
type=file
~~~
# SOURCE
~~~roc
f = |value| (value.convert(), value.convert())
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,Comma,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-tuple
					(e-method-call (method ".convert")
						(receiver
							(e-ident (raw "value")))
						(args))
					(e-method-call (method ".convert")
						(receiver
							(e-ident (raw "value")))
						(args)))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-tuple
				(elems
					(e-dispatch-call (method "convert") (constraint-fn-var 209)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args))
					(e-dispatch-call (method "convert") (constraint-fn-var 211)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> (b, c) where [a.convert : a -> c, a.convert : a -> b]")))
	(expressions
		(expr (type "a -> (b, c) where [a.convert : a -> c, a.convert : a -> b]"))))
~~~
