# META
~~~ini
description=Simple addition function with expect statement
type=file
~~~
# SOURCE
~~~roc
module [addU8]

addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:14),CloseSquare(1:14-1:15),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),UpperIdent(3:9-3:11),Comma(3:11-3:12),UpperIdent(3:13-3:15),OpArrow(3:16-3:18),UpperIdent(3:19-3:21),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:11),Comma(4:11-4:12),LowerIdent(4:13-4:14),OpBar(4:14-4:15),LowerIdent(4:16-4:17),OpPlus(4:18-4:19),LowerIdent(4:20-4:21),
KwExpect(6:1-6:7),LowerIdent(6:8-6:13),NoSpaceOpenRound(6:13-6:14),Int(6:14-6:15),Comma(6:15-6:16),Int(6:17-6:18),CloseRound(6:18-6:19),OpEquals(6:20-6:22),Int(6:23-6:24),
KwExpect(7:1-7:7),LowerIdent(7:8-7:13),NoSpaceOpenRound(7:13-7:14),Int(7:14-7:15),Comma(7:15-7:16),Int(7:17-7:19),CloseRound(7:19-7:20),OpEquals(7:21-7:23),Int(7:24-7:26),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.26
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-lower-ident @1.9-1.14
				(text "addU8"))))
	(statements
		(s-type-anno @3.1-3.21 (name "addU8")
			(ty-fn @3.9-3.21
				(ty @3.9-3.11 (name "U8"))
				(ty @3.13-3.15 (name "U8"))
				(ty @3.19-3.21 (name "U8"))))
		(s-decl @4.1-4.21
			(p-ident @4.1-4.6 (raw "addU8"))
			(e-lambda @4.9-4.21
				(args
					(p-ident @4.10-4.11 (raw "a"))
					(p-ident @4.13-4.14 (raw "b")))
				(e-binop @4.16-4.21 (op "+")
					(e-ident @4.16-4.17 (raw "a"))
					(e-ident @4.20-4.21 (raw "b")))))
		(s-expect @6.1-6.24
			(e-binop @6.8-6.24 (op "==")
				(e-apply @6.8-6.19
					(e-ident @6.8-6.13 (raw "addU8"))
					(e-int @6.14-6.15 (raw "1"))
					(e-int @6.17-6.18 (raw "2")))
				(e-int @6.23-6.24 (raw "3"))))
		(s-expect @7.1-7.26
			(e-binop @7.8-7.26 (op "==")
				(e-apply @7.8-7.20
					(e-ident @7.8-7.13 (raw "addU8"))
					(e-int @7.14-7.15 (raw "0"))
					(e-int @7.17-7.19 (raw "10")))
				(e-int @7.24-7.26 (raw "10"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "addU8"))
		(e-lambda @4.9-4.21
			(args
				(p-assign @4.10-4.11 (ident "a"))
				(p-assign @4.13-4.14 (ident "b")))
			(e-binop @4.16-4.21 (op "add")
				(e-lookup-local @4.16-4.17
					(p-assign @4.10-4.11 (ident "a")))
				(e-lookup-local @4.20-4.21
					(p-assign @4.13-4.14 (ident "b")))))
		(annotation @4.1-4.6
			(declared-type
				(ty-fn @3.9-3.21 (effectful false)
					(ty-lookup @3.9-3.11 (name "U8") (builtin))
					(ty-lookup @3.13-3.15 (name "U8") (builtin))
					(ty-lookup @3.19-3.21 (name "U8") (builtin))))))
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
			(tag_name @1.1-1.1 (name "Err"))))
	(s-expect @6.1-6.24
		(e-binop @6.8-6.24 (op "eq")
			(e-call @6.8-6.19
				(e-num @6.14-6.15 (value "1"))
				(e-num @6.17-6.18 (value "2")))
			(e-num @6.23-6.24 (value "3"))))
	(s-expect @7.1-7.26
		(e-binop @7.8-7.26 (op "eq")
			(e-call @7.8-7.20
				(e-num @7.14-7.15 (value "0"))
				(e-num @7.17-7.19 (value "10")))
			(e-num @7.24-7.26 (value "10")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "Num(Int(Unsigned8)), Num(Int(Unsigned8)) -> Num(Int(Unsigned8))")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @4.9-4.21 (type "Num(Int(Unsigned8)), Num(Int(Unsigned8)) -> Num(Int(Unsigned8))"))))
~~~
