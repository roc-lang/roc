# META
~~~ini
description=Mono test: identity function applied to integer at top-level
type=mono
~~~
# SOURCE
~~~roc
identity = |x| x
result = identity(42)
~~~
# MONO
~~~roc
identity : _a -> _a = |x| x
result : Dec = 42
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "identity"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "identity"))
				(e-int (raw "42"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "identity"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a -> a"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
