# META
~~~ini
description=Type annotation connection to definitions
type=snippet
~~~
# SOURCE
~~~roc
add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),UpperIdent(1:11-1:14),OpArrow(1:15-1:17),UpperIdent(1:18-1:21),
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),OpBar(2:11-2:12),LowerIdent(2:12-2:13),OpBar(2:13-2:14),LowerIdent(2:15-2:16),OpPlus(2:17-2:18),Int(2:19-2:20),
LowerIdent(4:1-4:10),OpColon(4:11-4:12),UpperIdent(4:13-4:16),
LowerIdent(5:1-5:10),OpAssign(5:11-5:12),LowerIdent(5:13-5:20),NoSpaceOpenRound(5:20-5:21),Int(5:21-5:23),CloseRound(5:23-5:24),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.24
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-1.21 (name "add_one")
			(ty-fn @1.11-1.21
				(ty @1.11-1.14 (name "U64"))
				(ty @1.18-1.21 (name "U64"))))
		(s-decl @2.1-2.20
			(p-ident @2.1-2.8 (raw "add_one"))
			(e-lambda @2.11-2.20
				(args
					(p-ident @2.12-2.13 (raw "x")))
				(e-binop @2.15-2.20 (op "+")
					(e-ident @2.15-2.16 (raw "x"))
					(e-int @2.19-2.20 (raw "1")))))
		(s-type-anno @4.1-4.16 (name "my_number")
			(ty @4.13-4.16 (name "U64")))
		(s-decl @5.1-5.24
			(p-ident @5.1-5.10 (raw "my_number"))
			(e-apply @5.13-5.24
				(e-ident @5.13-5.20 (raw "add_one"))
				(e-int @5.21-5.23 (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.8 (ident "add_one"))
		(e-lambda @2.11-2.20
			(args
				(p-assign @2.12-2.13 (ident "x")))
			(e-binop @2.15-2.20 (op "add")
				(e-lookup-local @2.15-2.16
					(p-assign @2.12-2.13 (ident "x")))
				(e-num @2.19-2.20 (value "1"))))
		(annotation @2.1-2.8
			(declared-type
				(ty-fn @1.11-1.21 (effectful false)
					(ty-lookup @1.11-1.14 (name "U64") (builtin))
					(ty-lookup @1.18-1.21 (name "U64") (builtin))))))
	(d-let
		(p-assign @5.1-5.10 (ident "my_number"))
		(e-call @5.13-5.24
			(e-lookup-local @5.13-5.20
				(p-assign @2.1-2.8 (ident "add_one")))
			(e-num @5.21-5.23 (value "42")))
		(annotation @5.1-5.10
			(declared-type
				(ty-lookup @4.13-4.16 (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.8 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt @5.1-5.10 (type "Num(Int(Unsigned64))")))
	(expressions
		(expr @2.11-2.20 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr @5.13-5.24 (type "Num(Int(Unsigned64))"))))
~~~
