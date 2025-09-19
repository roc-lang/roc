# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:18),OpBar(4:18-4:19),OpenCurly(4:20-4:21),
LowerIdent(5:2-5:5),OpAssign(5:6-5:7),LowerIdent(5:8-5:13),
KwVar(6:2-6:5),LowerIdent(6:6-6:10),OpAssign(6:11-6:12),LowerIdent(6:13-6:18),OpStar(6:19-6:20),Int(6:21-6:22),
LowerIdent(8:2-8:6),OpAssign(8:7-8:8),LowerIdent(8:9-8:13),OpPlus(8:14-8:15),LowerIdent(8:16-8:19),
LowerIdent(9:2-9:5),OpPlus(9:6-9:7),LowerIdent(9:8-9:12),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @4.1-10.2
			(p-ident @4.1-4.9 (raw "testFunc"))
			(e-lambda @4.12-10.2
				(args
					(p-ident @4.13-4.18 (raw "input")))
				(e-block @4.20-10.2
					(statements
						(s-decl @5.2-5.13
							(p-ident @5.2-5.5 (raw "sum"))
							(e-ident @5.8-5.13 (raw "input")))
						(s-var @6.2-6.22 (name "sum_")
							(e-binop @6.13-6.22 (op "*")
								(e-ident @6.13-6.18 (raw "input"))
								(e-int @6.21-6.22 (raw "2"))))
						(s-decl @8.2-8.19
							(p-ident @8.2-8.6 (raw "sum_"))
							(e-binop @8.9-8.19 (op "+")
								(e-ident @8.9-8.13 (raw "sum_"))
								(e-ident @8.16-8.19 (raw "sum"))))
						(e-binop @9.2-9.12 (op "+")
							(e-ident @9.2-9.5 (raw "sum"))
							(e-ident @9.8-9.12 (raw "sum_")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.9 (ident "testFunc"))
		(e-lambda @4.12-10.2
			(args
				(p-assign @4.13-4.18 (ident "input")))
			(e-block @4.20-10.2
				(s-let @5.2-5.13
					(p-assign @5.2-5.5 (ident "sum"))
					(e-lookup-local @5.8-5.13
						(p-assign @4.13-4.18 (ident "input"))))
				(s-var @6.2-6.22
					(p-assign @6.2-6.22 (ident "sum_"))
					(e-binop @6.13-6.22 (op "mul")
						(e-lookup-local @6.13-6.18
							(p-assign @4.13-4.18 (ident "input")))
						(e-num @6.21-6.22 (value "2"))))
				(s-reassign @8.2-8.6
					(p-assign @6.2-6.22 (ident "sum_"))
					(e-binop @8.9-8.19 (op "add")
						(e-lookup-local @8.9-8.13
							(p-assign @6.2-6.22 (ident "sum_")))
						(e-lookup-local @8.16-8.19
							(p-assign @5.2-5.5 (ident "sum")))))
				(e-binop @9.2-9.12 (op "add")
					(e-lookup-local @9.2-9.5
						(p-assign @5.2-5.5 (ident "sum")))
					(e-lookup-local @9.8-9.12
						(p-assign @6.2-6.22 (ident "sum_")))))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.9 (type "Num(_size) -> Num(_size2)")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @4.12-10.2 (type "Num(_size) -> Num(_size2)"))))
~~~
