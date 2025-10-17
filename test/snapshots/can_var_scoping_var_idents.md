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
	var sum_ = input * 2 # Var with underscore - should not conflict

	sum_ = sum_ + sum # Reassign var - should work
	sum + sum_ # Both should be accessible
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
						(s-var (name "sum_")
							(e-binop (op "*")
								(e-ident (raw "input"))
								(e-int (raw "2"))))
						(s-decl
							(p-ident (raw "sum_"))
							(e-binop (op "+")
								(e-ident (raw "sum_"))
								(e-ident (raw "sum"))))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-ident (raw "sum_")))))))))
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
					(p-assign (ident "sum_"))
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "input")))
						(e-num (value "2"))))
				(s-reassign
					(p-assign (ident "sum_"))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "sum_")))
						(e-lookup-local
							(p-assign (ident "sum")))))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "sum")))
					(e-lookup-local
						(p-assign (ident "sum_"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size) -> Num(_size2)")))
	(expressions
		(expr (type "Num(_size) -> Num(_size2)"))))
~~~
