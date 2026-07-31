# META
~~~ini
description=Pipe and legacy arrow postfix precedence
type=expr
~~~
# SOURCE
~~~roc
(foo |> bar(baz).blah(), foo->bar(baz).blah(), value |> (|x| x))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,LowerIdent,OpPizza,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,Comma,LowerIdent,OpPizza,OpenRound,OpBar,LowerIdent,OpBar,LowerIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-arrow-call
		(e-ident (raw "foo"))
		(e-method-call (method ".blah")
			(receiver
				(e-apply
					(e-ident (raw "bar"))
					(e-ident (raw "baz"))))
			(args)))
	(e-method-call (method ".blah")
		(receiver
			(e-arrow-call
				(e-ident (raw "foo"))
				(e-apply
					(e-ident (raw "bar"))
					(e-ident (raw "baz")))))
		(args))
	(e-arrow-call
		(e-ident (raw "value"))
		(e-lambda
			(args
				(p-ident (raw "x")))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
(foo |> bar(baz).blah(), (foo |> bar(baz)).blah(), value |> (|x| x))
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-call
			(e-method-call (method "blah")
				(receiver
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-runtime-error (tag "ident_not_in_scope"))))
				(args))
			(e-runtime-error (tag "ident_not_in_scope")))
		(e-method-call (method "blah")
			(receiver
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-runtime-error (tag "ident_not_in_scope"))))
			(args))
		(e-call
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-lookup-local
					(p-assign (ident "x"))))
			(e-runtime-error (tag "ident_not_in_scope")))))
~~~
# TYPES
~~~clojure
(expr (type "(Error, Error, Error)"))
~~~
