# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Function showing var vs regular identifier independence
testFunc = |input| {
	sum = input # Regular identifier
	var $sum = input * 2 # Var with $ prefix - should not conflict

	$sum = $sum + sum # Reassign var - should work
	sum + $sum # Both should be accessible
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
KwVar,LowerIdent,OpAssign,LowerIdent,OpStar,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "testFunc"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "sum"))
							(e-ident (raw "input")))
						(s-var (name "$sum")
							(e-binop (op "*")
								(e-ident (raw "input"))
								(e-int (raw "2"))))
						(s-decl
							(p-ident (raw "$sum"))
							(e-binop (op "+")
								(e-ident (raw "$sum"))
								(e-ident (raw "sum"))))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-ident (raw "$sum")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "testFunc"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-block
				(s-let
					(p-assign (ident "sum"))
					(e-lookup-local
						(p-assign (ident "input"))))
				(s-var
					(p-assign (ident "$sum"))
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "input")))
						(e-num (value "2"))))
				(s-reassign
					(p-assign (ident "$sum"))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "$sum")))
						(e-lookup-local
							(p-assign (ident "sum")))))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "sum")))
					(e-lookup-local
						(p-assign (ident "$sum"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.plus : a, a -> a, a.times : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a -> a where [a.plus : a, a -> a, a.times : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
