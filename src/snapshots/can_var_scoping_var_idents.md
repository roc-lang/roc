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
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:58),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:18),OpBar(4:18-4:19),OpenCurly(4:20-4:21),Newline(1:1-1:1),
LowerIdent(5:2-5:5),OpAssign(5:6-5:7),LowerIdent(5:8-5:13),Newline(5:15-5:34),
KwVar(6:2-6:5),LowerIdent(6:6-6:10),OpAssign(6:11-6:12),LowerIdent(6:13-6:18),OpStar(6:19-6:20),Int(6:21-6:22),Newline(6:24-6:66),
Newline(1:1-1:1),
LowerIdent(8:2-8:6),OpAssign(8:7-8:8),LowerIdent(8:9-8:13),OpPlus(8:14-8:15),LowerIdent(8:16-8:19),Newline(8:21-8:48),
LowerIdent(9:2-9:5),OpPlus(9:6-9:7),LowerIdent(9:8-9:12),Newline(9:14-9:40),
CloseCurly(10:1-10:2),EndOfFile(10:2-10:2),
~~~
# PARSE
~~~clojure
(file @1-1-10-2
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-decl @4-1-10-2
			(p-ident @4-1-4-9 (raw "testFunc"))
			(e-lambda @4-12-10-2
				(args
					(p-ident @4-13-4-18 (raw "input")))
				(e-block @4-20-10-2
					(statements
						(s-decl @5-2-5-13
							(p-ident @5-2-5-5 (raw "sum"))
							(e-ident @5-8-5-13 (qaul "") (raw "input")))
						(s-var @6-2-8-6 (name "sum_")
							(e-binop @6-13-8-6 (op "*")
								(e-ident @6-13-6-18 (qaul "") (raw "input"))
								(e-int @6-21-6-22 (raw "2"))))
						(s-decl @8-2-9-5
							(p-ident @8-2-8-6 (raw "sum_"))
							(e-binop @8-9-9-5 (op "+")
								(e-ident @8-9-8-13 (qaul "") (raw "sum_"))
								(e-ident @8-16-8-19 (qaul "") (raw "sum"))))
						(e-binop @9-2-10-2 (op "+")
							(e-ident @9-2-9-5 (qaul "") (raw "sum"))
							(e-ident @9-8-9-12 (qaul "") (raw "sum_")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 92)
		(p-assign @4-1-4-9 (ident "testFunc") (id 72))
		(e-lambda @4-12-10-2 (id 91)
			(args
				(p-assign @4-13-4-18 (ident "input") (id 73)))
			(e-block @4-20-10-2
				(s-let @5-2-5-13
					(p-assign @5-2-5-5 (ident "sum") (id 74))
					(e-lookup-local @5-8-5-13 (id 75)
						(pattern (id 73))))
				(s-var @6-2-8-6
					(p-assign @6-2-8-6 (ident "sum_") (id 81))
					(e-binop @6-13-8-6 (op "mul") (id 80)
						(e-lookup-local @6-13-6-18
							(pattern (id 73)))
						(e-int @6-21-6-22 (value "2"))))
				(s-reassign @8-2-8-6
					(p-assign @6-2-8-6 (ident "sum_") (id 81))
					(e-binop @8-9-9-5 (op "add") (id 85)
						(e-lookup-local @8-9-8-13
							(pattern (id 81)))
						(e-lookup-local @8-16-8-19
							(pattern (id 74)))))
				(e-binop @9-2-10-2 (op "add")
					(e-lookup-local @9-2-9-5
						(pattern (id 74)))
					(e-lookup-local @9-8-9-12
						(pattern (id 81))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "testFunc") (type "*")))
	(expressions
		(expr @4-12-10-2 (type "*"))))
~~~