# META
~~~ini
description=Mono test: pure lambda (no captures) assigned to top-level
type=mono
~~~
# SOURCE
~~~roc
add_one = |x| x + 1
result = add_one(5)
~~~
# MONO
~~~roc
add_one : Dec -> Dec
add_one = #add_one_1({})
result : Dec
result = match add_one {
    #add_one_1({}) => {
        x = 5
        x + 1
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_one"))
				(e-int (raw "5"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add_one"))
		(e-tag (name "#add_one_1")
			(args
				(e-empty_record))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-lookup-local
						(p-assign (ident "add_one"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-block
								(s-let
									(p-assign (ident "x"))
									(e-num (value "5")))
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "x")))
									(e-num (value "1")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "[False, True]"))
		(expr (type "[Error]"))))
~~~
