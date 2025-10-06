# META
~~~ini
description=Lambda currying with polymorphic function constraints - tests if numeric literals in curried functions get properly constrained
type=snippet
~~~
# SOURCE
~~~roc
# Function that returns a function with polymorphic type
makeAdder : a -> (a -> a)
makeAdder = |x| |y| x + y

# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)

# Higher-order function that applies a function twice
applyTwice : (a -> a), a -> a
applyTwice = |f, x| f(f(x))

# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| x + 3, n)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(2:1-2:10),OpColon(2:11-2:12),LowerIdent(2:13-2:14),OpArrow(2:15-2:17),OpenRound(2:18-2:19),LowerIdent(2:19-2:20),OpArrow(2:21-2:23),LowerIdent(2:24-2:25),CloseRound(2:25-2:26),
LowerIdent(3:1-3:10),OpAssign(3:11-3:12),OpBar(3:13-3:14),LowerIdent(3:14-3:15),OpBar(3:15-3:16),OpBar(3:17-3:18),LowerIdent(3:18-3:19),OpBar(3:19-3:20),LowerIdent(3:21-3:22),OpPlus(3:23-3:24),LowerIdent(3:25-3:26),
LowerIdent(6:1-6:11),OpColon(6:12-6:13),UpperIdent(6:14-6:17),OpArrow(6:18-6:20),UpperIdent(6:21-6:24),
LowerIdent(7:1-7:11),OpAssign(7:12-7:13),LowerIdent(7:14-7:23),NoSpaceOpenRound(7:23-7:24),Int(7:24-7:25),CloseRound(7:25-7:26),
LowerIdent(10:1-10:11),OpColon(10:12-10:13),OpenRound(10:14-10:15),LowerIdent(10:15-10:16),OpArrow(10:17-10:19),LowerIdent(10:20-10:21),CloseRound(10:21-10:22),Comma(10:22-10:23),LowerIdent(10:24-10:25),OpArrow(10:26-10:28),LowerIdent(10:29-10:30),
LowerIdent(11:1-11:11),OpAssign(11:12-11:13),OpBar(11:14-11:15),LowerIdent(11:15-11:16),Comma(11:16-11:17),LowerIdent(11:18-11:19),OpBar(11:19-11:20),LowerIdent(11:21-11:22),NoSpaceOpenRound(11:22-11:23),LowerIdent(11:23-11:24),NoSpaceOpenRound(11:24-11:25),LowerIdent(11:25-11:26),CloseRound(11:26-11:27),CloseRound(11:27-11:28),
LowerIdent(14:1-14:14),OpColon(14:15-14:16),UpperIdent(14:17-14:20),OpArrow(14:21-14:23),UpperIdent(14:24-14:27),
LowerIdent(15:1-15:14),OpAssign(15:15-15:16),OpBar(15:17-15:18),LowerIdent(15:18-15:19),OpBar(15:19-15:20),LowerIdent(15:21-15:31),NoSpaceOpenRound(15:31-15:32),OpBar(15:32-15:33),LowerIdent(15:33-15:34),OpBar(15:34-15:35),LowerIdent(15:36-15:37),OpPlus(15:38-15:39),Int(15:40-15:41),Comma(15:41-15:42),LowerIdent(15:43-15:44),CloseRound(15:44-15:45),
EndOfFile(16:1-16:1),
~~~
# PARSE
~~~clojure
(file @2.1-15.45
	(type-module @2.1-2.10)
	(statements
		(s-type-anno @2.1-2.26 (name "makeAdder")
			(ty-fn @2.13-2.26
				(ty-var @2.13-2.14 (raw "a"))
				(ty-fn @2.19-2.25
					(ty-var @2.19-2.20 (raw "a"))
					(ty-var @2.24-2.25 (raw "a")))))
		(s-decl @3.1-3.26
			(p-ident @3.1-3.10 (raw "makeAdder"))
			(e-lambda @3.13-3.26
				(args
					(p-ident @3.14-3.15 (raw "x")))
				(e-lambda @3.17-3.26
					(args
						(p-ident @3.18-3.19 (raw "y")))
					(e-binop @3.21-3.26 (op "+")
						(e-ident @3.21-3.22 (raw "x"))
						(e-ident @3.25-3.26 (raw "y"))))))
		(s-type-anno @6.1-6.24 (name "curriedAdd")
			(ty-fn @6.14-6.24
				(ty @6.14-6.17 (name "I64"))
				(ty @6.21-6.24 (name "I64"))))
		(s-decl @7.1-7.26
			(p-ident @7.1-7.11 (raw "curriedAdd"))
			(e-apply @7.14-7.26
				(e-ident @7.14-7.23 (raw "makeAdder"))
				(e-int @7.24-7.25 (raw "5"))))
		(s-type-anno @10.1-10.30 (name "applyTwice")
			(ty-fn @10.14-10.30
				(ty-fn @10.15-10.21
					(ty-var @10.15-10.16 (raw "a"))
					(ty-var @10.20-10.21 (raw "a")))
				(ty-var @10.24-10.25 (raw "a"))
				(ty-var @10.29-10.30 (raw "a"))))
		(s-decl @11.1-11.28
			(p-ident @11.1-11.11 (raw "applyTwice"))
			(e-lambda @11.14-11.28
				(args
					(p-ident @11.15-11.16 (raw "f"))
					(p-ident @11.18-11.19 (raw "x")))
				(e-apply @11.21-11.28
					(e-ident @11.21-11.22 (raw "f"))
					(e-apply @11.23-11.27
						(e-ident @11.23-11.24 (raw "f"))
						(e-ident @11.25-11.26 (raw "x"))))))
		(s-type-anno @14.1-14.27 (name "addThreeTwice")
			(ty-fn @14.17-14.27
				(ty @14.17-14.20 (name "I64"))
				(ty @14.24-14.27 (name "I64"))))
		(s-decl @15.1-15.45
			(p-ident @15.1-15.14 (raw "addThreeTwice"))
			(e-lambda @15.17-15.45
				(args
					(p-ident @15.18-15.19 (raw "n")))
				(e-apply @15.21-15.45
					(e-ident @15.21-15.31 (raw "applyTwice"))
					(e-lambda @15.32-15.41
						(args
							(p-ident @15.33-15.34 (raw "x")))
						(e-binop @15.36-15.41 (op "+")
							(e-ident @15.36-15.37 (raw "x"))
							(e-int @15.40-15.41 (raw "3"))))
					(e-ident @15.43-15.44 (raw "n")))))))
~~~
# FORMATTED
~~~roc
# Function that returns a function with polymorphic type
# Function that returns a function with polymorphic type
makeAdder : a -> (a -> a)
makeAdder = |x| |y| x + y

# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)

# Higher-order function that applies a function twice
applyTwice : (a -> a), a -> a
applyTwice = |f, x| f(f(x))

# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| x + 3, n)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.10 (ident "makeAdder"))
		(e-lambda @3.13-3.26
			(args
				(p-assign @3.14-3.15 (ident "x")))
			(e-closure @3.17-3.26
				(captures
					(capture @3.14-3.15 (ident "x")))
				(e-lambda @3.17-3.26
					(args
						(p-assign @3.18-3.19 (ident "y")))
					(e-binop @3.21-3.26 (op "add")
						(e-lookup-local @3.21-3.22
							(p-assign @3.14-3.15 (ident "x")))
						(e-lookup-local @3.25-3.26
							(p-assign @3.18-3.19 (ident "y")))))))
		(annotation @3.1-3.10
			(declared-type
				(ty-fn @2.13-2.26 (effectful false)
					(ty-rigid-var @2.13-2.14 (name "a"))
					(ty-parens @2.18-2.26
						(ty-fn @2.19-2.25 (effectful false)
							(ty-rigid-var-lookup (ty-rigid-var @2.13-2.14 (name "a")))
							(ty-rigid-var-lookup (ty-rigid-var @2.13-2.14 (name "a")))))))))
	(d-let
		(p-assign @7.1-7.11 (ident "curriedAdd"))
		(e-call @7.14-7.26
			(e-lookup-local @7.14-7.23
				(p-assign @3.1-3.10 (ident "makeAdder")))
			(e-num @7.24-7.25 (value "5")))
		(annotation @7.1-7.11
			(declared-type
				(ty-fn @6.14-6.24 (effectful false)
					(ty-lookup @6.14-6.17 (name "I64") (builtin))
					(ty-lookup @6.21-6.24 (name "I64") (builtin))))))
	(d-let
		(p-assign @11.1-11.11 (ident "applyTwice"))
		(e-lambda @11.14-11.28
			(args
				(p-assign @11.15-11.16 (ident "f"))
				(p-assign @11.18-11.19 (ident "x")))
			(e-call @11.21-11.28
				(e-lookup-local @11.21-11.22
					(p-assign @11.15-11.16 (ident "f")))
				(e-call @11.23-11.27
					(e-lookup-local @11.23-11.24
						(p-assign @11.15-11.16 (ident "f")))
					(e-lookup-local @11.25-11.26
						(p-assign @11.18-11.19 (ident "x"))))))
		(annotation @11.1-11.11
			(declared-type
				(ty-fn @10.14-10.30 (effectful false)
					(ty-parens @10.14-10.22
						(ty-fn @10.15-10.21 (effectful false)
							(ty-rigid-var @10.15-10.16 (name "a"))
							(ty-rigid-var-lookup (ty-rigid-var @10.15-10.16 (name "a")))))
					(ty-rigid-var-lookup (ty-rigid-var @10.15-10.16 (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var @10.15-10.16 (name "a")))))))
	(d-let
		(p-assign @15.1-15.14 (ident "addThreeTwice"))
		(e-closure @15.17-15.45
			(captures
				(capture @11.1-11.11 (ident "applyTwice")))
			(e-lambda @15.17-15.45
				(args
					(p-assign @15.18-15.19 (ident "n")))
				(e-call @15.21-15.45
					(e-lookup-local @15.21-15.31
						(p-assign @11.1-11.11 (ident "applyTwice")))
					(e-lambda @15.32-15.41
						(args
							(p-assign @15.33-15.34 (ident "x")))
						(e-binop @15.36-15.41 (op "add")
							(e-lookup-local @15.36-15.37
								(p-assign @15.33-15.34 (ident "x")))
							(e-num @15.40-15.41 (value "3"))))
					(e-lookup-local @15.43-15.44
						(p-assign @15.18-15.19 (ident "n"))))))
		(annotation @15.1-15.14
			(declared-type
				(ty-fn @14.17-14.27 (effectful false)
					(ty-lookup @14.17-14.20 (name "I64") (builtin))
					(ty-lookup @14.24-14.27 (name "I64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.10 (type "a -> a -> a"))
		(patt @7.1-7.11 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(patt @11.1-11.11 (type "a -> a, a -> a"))
		(patt @15.1-15.14 (type "Num(Int(Signed64)) -> Num(Int(Signed64))")))
	(expressions
		(expr @3.13-3.26 (type "a -> a -> a"))
		(expr @7.14-7.26 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(expr @11.14-11.28 (type "a -> a, a -> a"))
		(expr @15.17-15.45 (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))))
~~~
