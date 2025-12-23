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
identity : [False, True]
identity = #identity_1({})
result : Dec
result = match identity {
    #identity_1({}) => {
        x = 42
        x
    },
}
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
		(e-tag (name "#identity_1")
			(args
				(e-empty_record))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-lookup-local
						(p-assign (ident "identity"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-block
								(s-let
									(p-assign (ident "x"))
									(e-num (value "42")))
								(e-lookup-local
									(p-assign (ident "x")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "[False, True]"))
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
