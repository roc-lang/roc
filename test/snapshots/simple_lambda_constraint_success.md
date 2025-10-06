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
LowerIdent(2:1-2:7),OpColon(2:8-2:9),UpperIdent(2:10-2:13),OpArrow(2:14-2:16),UpperIdent(2:17-2:20),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),OpBar(3:10-3:11),LowerIdent(3:11-3:12),OpBar(3:12-3:13),LowerIdent(3:14-3:15),OpPlus(3:16-3:17),Int(3:18-3:19),
LowerIdent(6:1-6:10),OpColon(6:11-6:12),UpperIdent(6:13-6:16),OpArrow(6:17-6:19),UpperIdent(6:20-6:23),
LowerIdent(7:1-7:10),OpAssign(7:11-7:12),OpBar(7:13-7:14),LowerIdent(7:14-7:15),OpBar(7:15-7:16),LowerIdent(7:17-7:18),OpPlus(7:19-7:20),Float(7:21-7:24),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @2.1-7.24
	(type-module @2.1-2.7)
	(statements
		(s-type-anno @2.1-2.20 (name "addTwo")
			(ty-fn @2.10-2.20
				(ty @2.10-2.13 (name "I64"))
				(ty @2.17-2.20 (name "I64"))))
		(s-decl @3.1-3.19
			(p-ident @3.1-3.7 (raw "addTwo"))
			(e-lambda @3.10-3.19
				(args
					(p-ident @3.11-3.12 (raw "x")))
				(e-binop @3.14-3.19 (op "+")
					(e-ident @3.14-3.15 (raw "x"))
					(e-int @3.18-3.19 (raw "2")))))
		(s-type-anno @6.1-6.23 (name "addTwoF64")
			(ty-fn @6.13-6.23
				(ty @6.13-6.16 (name "F64"))
				(ty @6.20-6.23 (name "F64"))))
		(s-decl @7.1-7.24
			(p-ident @7.1-7.10 (raw "addTwoF64"))
			(e-lambda @7.13-7.24
				(args
					(p-ident @7.14-7.15 (raw "x")))
				(e-binop @7.17-7.24 (op "+")
					(e-ident @7.17-7.18 (raw "x"))
					(e-frac @7.21-7.24 (raw "2.0")))))))
~~~
# FORMATTED
~~~roc
# Should successfully constrain literal 2 to I64
# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2

# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.7 (ident "addTwo"))
		(e-lambda @3.10-3.19
			(args
				(p-assign @3.11-3.12 (ident "x")))
			(e-binop @3.14-3.19 (op "add")
				(e-lookup-local @3.14-3.15
					(p-assign @3.11-3.12 (ident "x")))
				(e-num @3.18-3.19 (value "2"))))
		(annotation @3.1-3.7
			(declared-type
				(ty-fn @2.10-2.20 (effectful false)
					(ty-lookup @2.10-2.13 (name "I64") (builtin))
					(ty-lookup @2.17-2.20 (name "I64") (builtin))))))
	(d-let
		(p-assign @7.1-7.10 (ident "addTwoF64"))
		(e-lambda @7.13-7.24
			(args
				(p-assign @7.14-7.15 (ident "x")))
			(e-binop @7.17-7.24 (op "add")
				(e-lookup-local @7.17-7.18
					(p-assign @7.14-7.15 (ident "x")))
				(e-dec-small @7.21-7.24 (numerator "20") (denominator-power-of-ten "1") (value "2"))))
		(annotation @7.1-7.10
			(declared-type
				(ty-fn @6.13-6.23 (effectful false)
					(ty-lookup @6.13-6.16 (name "F64") (builtin))
					(ty-lookup @6.20-6.23 (name "F64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(patt @7.1-7.10 (type "Num(Frac(Float64)) -> Num(Frac(Float64))")))
	(expressions
		(expr @3.10-3.19 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(expr @7.13-7.24 (type "Num(Frac(Float64)) -> Num(Frac(Float64))"))))
~~~
