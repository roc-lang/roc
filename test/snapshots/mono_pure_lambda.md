# META
~~~ini
description=Mono test: pure lambda (no captures) transforms to tag with empty record
type=mono
~~~
# SOURCE
~~~roc
|x| x + 1
~~~
# MONO
~~~roc
|x| x + 1 : Dec -> Dec
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-binop (op "+")
		(e-ident (raw "x"))
		(e-int (raw "1"))))
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-binop (op "add")
		(e-lookup-local
			(p-assign (ident "x")))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
