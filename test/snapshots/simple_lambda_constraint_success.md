# META
~~~ini
description=Simple lambda constraint success test - verifies bidirectional type checking works correctly
type=file:SimpleLambdaConstraintSuccess.roc
~~~
# SOURCE
~~~roc
SimpleLambdaConstraintSuccess := {}

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
UpperIdent(1:1-1:30),OpColonEqual(1:31-1:33),OpenCurly(1:34-1:35),CloseCurly(1:35-1:36),
LowerIdent(4:1-4:7),OpColon(4:8-4:9),UpperIdent(4:10-4:13),OpArrow(4:14-4:16),UpperIdent(4:17-4:20),
LowerIdent(5:1-5:7),OpAssign(5:8-5:9),OpBar(5:10-5:11),LowerIdent(5:11-5:12),OpBar(5:12-5:13),LowerIdent(5:14-5:15),OpPlus(5:16-5:17),Int(5:18-5:19),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:16),OpArrow(8:17-8:19),UpperIdent(8:20-8:23),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),LowerIdent(9:14-9:15),OpBar(9:15-9:16),LowerIdent(9:17-9:18),OpPlus(9:19-9:20),Float(9:21-9:24),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.24
	(type-module @1.1-1.30)
	(statements
		(s-type-decl @1.1-1.36
			(header @1.1-1.30 (name "SimpleLambdaConstraintSuccess")
				(args))
			(ty-record @1.34-1.36))
		(s-type-anno @4.1-4.20 (name "addTwo")
			(ty-fn @4.10-4.20
				(ty @4.10-4.13 (name "I64"))
				(ty @4.17-4.20 (name "I64"))))
		(s-decl @5.1-5.19
			(p-ident @5.1-5.7 (raw "addTwo"))
			(e-lambda @5.10-5.19
				(args
					(p-ident @5.11-5.12 (raw "x")))
				(e-binop @5.14-5.19 (op "+")
					(e-ident @5.14-5.15 (raw "x"))
					(e-int @5.18-5.19 (raw "2")))))
		(s-type-anno @8.1-8.23 (name "addTwoF64")
			(ty-fn @8.13-8.23
				(ty @8.13-8.16 (name "F64"))
				(ty @8.20-8.23 (name "F64"))))
		(s-decl @9.1-9.24
			(p-ident @9.1-9.10 (raw "addTwoF64"))
			(e-lambda @9.13-9.24
				(args
					(p-ident @9.14-9.15 (raw "x")))
				(e-binop @9.17-9.24 (op "+")
					(e-ident @9.17-9.18 (raw "x"))
					(e-frac @9.21-9.24 (raw "2.0")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.7 (ident "addTwo"))
		(e-lambda @5.10-5.19
			(args
				(p-assign @5.11-5.12 (ident "x")))
			(e-binop @5.14-5.19 (op "add")
				(e-lookup-local @5.14-5.15
					(p-assign @5.11-5.12 (ident "x")))
				(e-num @5.18-5.19 (value "2"))))
		(annotation @5.1-5.7
			(declared-type
				(ty-fn @4.10-4.20 (effectful false)
					(ty-lookup @4.10-4.13 (name "I64") (builtin))
					(ty-lookup @4.17-4.20 (name "I64") (builtin))))))
	(d-let
		(p-assign @9.1-9.10 (ident "addTwoF64"))
		(e-lambda @9.13-9.24
			(args
				(p-assign @9.14-9.15 (ident "x")))
			(e-binop @9.17-9.24 (op "add")
				(e-lookup-local @9.17-9.18
					(p-assign @9.14-9.15 (ident "x")))
				(e-dec-small @9.21-9.24 (numerator "20") (denominator-power-of-ten "1") (value "2"))))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.23 (effectful false)
					(ty-lookup @8.13-8.16 (name "F64") (builtin))
					(ty-lookup @8.20-8.23 (name "F64") (builtin))))))
	(s-nominal-decl @1.1-1.36
		(ty-header @1.1-1.30 (name "SimpleLambdaConstraintSuccess"))
		(ty-record @1.34-1.36)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.7 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(patt @9.1-9.10 (type "Num(Frac(Float64)) -> Num(Frac(Float64))")))
	(type_decls
		(nominal @1.1-1.36 (type "SimpleLambdaConstraintSuccess")
			(ty-header @1.1-1.30 (name "SimpleLambdaConstraintSuccess"))))
	(expressions
		(expr @5.10-5.19 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(expr @9.13-9.24 (type "Num(Frac(Float64)) -> Num(Frac(Float64))"))))
~~~
