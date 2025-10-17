# META
~~~ini
description=Simple lambda constraint success test - verifies bidirectional type checking works correctly
type=snippet
~~~
# SOURCE
~~~roc
# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2

# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "addTwo")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "addTwo"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "2")))))
		(s-type-anno (name "addTwoF64")
			(ty-fn
				(ty (name "F64"))
				(ty (name "F64"))))
		(s-decl
			(p-ident (raw "addTwoF64"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-frac (raw "2.0")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "addTwo"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "2"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin))))))
	(d-let
		(p-assign (ident "addTwoF64"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-dec-small (numerator "20") (denominator-power-of-ten "1") (value "2"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "F64") (builtin))
					(ty-lookup (name "F64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(patt (type "Num(Frac(Float64)) -> Num(Frac(Float64))")))
	(expressions
		(expr (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(expr (type "Num(Frac(Float64)) -> Num(Frac(Float64))"))))
~~~
