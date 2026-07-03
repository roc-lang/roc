# META
~~~ini
description=Nominal-value destructuring (Type.(pat)) on the LHS of a = definition
type=snippet
~~~
# SOURCE
~~~roc
Distance := U64

double : Distance -> U64
double = |d| {
    Distance.(n) = d
    n * 2
}

Distance.(five) = Distance.(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,LowerIdent,
LowerIdent,OpStar,Int,
CloseCurly,
UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpAssign,UpperIdent,Dot,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Distance")
				(args))
			(ty (name "U64")))
		(s-type-anno (name "double")
			(ty-fn
				(ty (name "Distance"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "double"))
			(e-lambda
				(args
					(p-ident (raw "d")))
				(e-block
					(statements
						(s-decl
							(p-tag (raw "Distance")
								(p-ident (raw "n")))
							(e-ident (raw "d")))
						(e-binop (op "*")
							(e-ident (raw "n"))
							(e-int (raw "2")))))))
		(s-decl
			(p-tag (raw "Distance")
				(p-ident (raw "five")))
			(e-nominal-apply
				(mapper (e-tag (raw "Distance")))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
Distance := U64

double : Distance -> U64
double = |d| {
	Distance.(n) = d
	n * 2
}

Distance.(five) = Distance.(5)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "double"))
		(e-lambda
			(args
				(p-assign (ident "d")))
			(e-block
				(s-let
					(p-nominal
						(p-assign (ident "n")))
					(e-lookup-local
						(p-assign (ident "d"))))
				(e-dispatch-call (method "times") (constraint-fn-var 84)
					(receiver
						(e-lookup-local
							(p-assign (ident "n"))))
					(args
						(e-num (value "2"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Distance") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-nominal
			(p-assign (ident "five")))
		(e-nominal (nominal "Distance")
			(e-num (value "5"))))
	(s-nominal-decl
		(ty-header (name "Distance"))
		(ty-lookup (name "U64") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Distance -> U64")))
	(type_decls
		(nominal (type "Distance")
			(ty-header (name "Distance"))))
	(expressions
		(expr (type "Distance -> U64"))
		(expr (type "Distance"))))
~~~
