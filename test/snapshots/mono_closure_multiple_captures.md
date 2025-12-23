# META
~~~ini
description=Mono test: closure with multiple captures at top-level
type=mono
~~~
# SOURCE
~~~roc
a = 1
b = 2
add_ab = |x| a + b + x
result = add_ab(10)
~~~
# MONO
~~~roc
a : Dec
a = 1
b : Dec
b = 2
add_ab : []
add_ab = #add_ab_1({a: a, b: b})
result : [Error]
result = match add_ab {
    #add_ab_1({a, b}) => {
        x = 10
        a + b + x
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
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-int (raw "1")))
		(s-decl
			(p-ident (raw "b"))
			(e-int (raw "2")))
		(s-decl
			(p-ident (raw "add_ab"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "a"))
						(e-ident (raw "b")))
					(e-ident (raw "x")))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_ab"))
				(e-int (raw "10"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "b"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "add_ab"))
		(e-tag (name "#add_ab_1")
			(args
				(e-record
					(fields
						(field (name "a")
							(e-lookup-local
								(p-assign (ident "a"))))
						(field (name "b")
							(e-lookup-local
								(p-assign (ident "b")))))))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-lookup-local
						(p-assign (ident "add_ab"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-block
								(s-let
									(p-assign (ident "x"))
									(e-num (value "10")))
								(e-binop (op "add")
									(e-binop (op "add")
										(e-lookup-local
											(p-assign (ident "a")))
										(e-lookup-local
											(p-assign (ident "b"))))
									(e-lookup-local
										(p-assign (ident "x"))))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "[]"))
		(expr (type "[Error]"))))
~~~
