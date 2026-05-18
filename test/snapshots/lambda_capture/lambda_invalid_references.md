# META
~~~ini
description=Error handling for invalid variable references in lambda captures
type=expr
~~~
# SOURCE
~~~roc
|x| |y| x + z
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-lambda
		(args
			(p-ident (raw "y")))
		(e-binop (op "+")
			(e-ident (raw "x"))
			(e-ident (raw "z")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-closure
		(captures
			(capture (ident "x")))
		(e-lambda
			(args
				(p-assign (ident "y")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-runtime-error (tag "ident_not_in_scope"))))))
~~~
# TYPES
~~~clojure
(expr (type "a -> (_arg -> a) where [a.plus : a, Error -> a]"))
~~~
