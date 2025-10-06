# META
~~~ini
description=Type annotation connection to definitions
type=file:TypeAnnoConnection.roc
~~~
# SOURCE
~~~roc
TypeAnnoConnection := {}

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
UpperIdent(1:1-1:19),OpColonEqual(1:20-1:22),OpenCurly(1:23-1:24),CloseCurly(1:24-1:25),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),UpperIdent(3:11-3:14),OpArrow(3:15-3:17),UpperIdent(3:18-3:21),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:13),OpBar(4:13-4:14),LowerIdent(4:15-4:16),OpPlus(4:17-4:18),Int(4:19-4:20),
LowerIdent(6:1-6:10),OpColon(6:11-6:12),UpperIdent(6:13-6:16),
LowerIdent(7:1-7:10),OpAssign(7:11-7:12),LowerIdent(7:13-7:20),NoSpaceOpenRound(7:20-7:21),Int(7:21-7:23),CloseRound(7:23-7:24),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.24
	(type-module @1.1-1.19)
	(statements
		(s-type-decl @1.1-1.25
			(header @1.1-1.19 (name "TypeAnnoConnection")
				(args))
			(ty-record @1.23-1.25))
		(s-type-anno @3.1-3.21 (name "add_one")
			(ty-fn @3.11-3.21
				(ty @3.11-3.14 (name "U64"))
				(ty @3.18-3.21 (name "U64"))))
		(s-decl @4.1-4.20
			(p-ident @4.1-4.8 (raw "add_one"))
			(e-lambda @4.11-4.20
				(args
					(p-ident @4.12-4.13 (raw "x")))
				(e-binop @4.15-4.20 (op "+")
					(e-ident @4.15-4.16 (raw "x"))
					(e-int @4.19-4.20 (raw "1")))))
		(s-type-anno @6.1-6.16 (name "my_number")
			(ty @6.13-6.16 (name "U64")))
		(s-decl @7.1-7.24
			(p-ident @7.1-7.10 (raw "my_number"))
			(e-apply @7.13-7.24
				(e-ident @7.13-7.20 (raw "add_one"))
				(e-int @7.21-7.23 (raw "42"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "add_one"))
		(e-lambda @4.11-4.20
			(args
				(p-assign @4.12-4.13 (ident "x")))
			(e-binop @4.15-4.20 (op "add")
				(e-lookup-local @4.15-4.16
					(p-assign @4.12-4.13 (ident "x")))
				(e-num @4.19-4.20 (value "1"))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.21 (effectful false)
					(ty-lookup @3.11-3.14 (name "U64") (builtin))
					(ty-lookup @3.18-3.21 (name "U64") (builtin))))))
	(d-let
		(p-assign @7.1-7.10 (ident "my_number"))
		(e-call @7.13-7.24
			(e-lookup-local @7.13-7.20
				(p-assign @4.1-4.8 (ident "add_one")))
			(e-num @7.21-7.23 (value "42")))
		(annotation @7.1-7.10
			(declared-type
				(ty-lookup @6.13-6.16 (name "U64") (builtin)))))
	(s-nominal-decl @1.1-1.25
		(ty-header @1.1-1.19 (name "TypeAnnoConnection"))
		(ty-record @1.23-1.25)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(patt @7.1-7.10 (type "Num(Int(Unsigned64))")))
	(type_decls
		(nominal @1.1-1.25 (type "TypeAnnoConnection")
			(ty-header @1.1-1.19 (name "TypeAnnoConnection"))))
	(expressions
		(expr @4.11-4.20 (type "Num(Int(Unsigned64)) -> Num(Int(Unsigned64))"))
		(expr @7.13-7.24 (type "Num(Int(Unsigned64))"))))
~~~
