# META
~~~ini
description=Record destructuring in assignment statement
type=snippet
~~~
# SOURCE
~~~roc
extract_age : { age : U64 } -> U64
extract_age = |person| {
    { age } = person

	{ a: 0 }.a + age - { a: 0 }.a
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:12),OpColon(1:13-1:14),OpenCurly(1:15-1:16),LowerIdent(1:17-1:20),OpColon(1:21-1:22),UpperIdent(1:23-1:26),CloseCurly(1:27-1:28),OpArrow(1:29-1:31),UpperIdent(1:32-1:35),
LowerIdent(2:1-2:12),OpAssign(2:13-2:14),OpBar(2:15-2:16),LowerIdent(2:16-2:22),OpBar(2:22-2:23),OpenCurly(2:24-2:25),
OpenCurly(3:5-3:6),LowerIdent(3:7-3:10),CloseCurly(3:11-3:12),OpAssign(3:13-3:14),LowerIdent(3:15-3:21),
OpenCurly(5:2-5:3),LowerIdent(5:4-5:5),OpColon(5:5-5:6),Int(5:7-5:8),CloseCurly(5:9-5:10),NoSpaceDotLowerIdent(5:10-5:12),OpPlus(5:13-5:14),LowerIdent(5:15-5:18),OpBinaryMinus(5:19-5:20),OpenCurly(5:21-5:22),LowerIdent(5:23-5:24),OpColon(5:24-5:25),Int(5:26-5:27),CloseCurly(5:28-5:29),NoSpaceDotLowerIdent(5:29-5:31),
CloseCurly(6:1-6:2),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.2
	(type-module @1.1-1.12)
	(statements
		(s-type-anno @1.1-1.35 (name "extract_age")
			(ty-fn @1.15-1.35
				(ty-record @1.15-1.28
					(anno-record-field @1.17-1.26 (name "age")
						(ty @1.23-1.26 (name "U64"))))
				(ty @1.32-1.35 (name "U64"))))
		(s-decl @2.1-6.2
			(p-ident @2.1-2.12 (raw "extract_age"))
			(e-lambda @2.15-6.2
				(args
					(p-ident @2.16-2.22 (raw "person")))
				(e-block @2.24-6.2
					(statements
						(s-decl @3.5-3.21
							(p-record @3.5-3.12
								(field @3.7-3.10 (name "age") (rest false)))
							(e-ident @3.15-3.21 (raw "person")))
						(e-binop @5.2-5.31 (op "-")
							(e-binop @5.2-5.18 (op "+")
								(e-field-access @5.2-5.12
									(e-record @5.2-5.10
										(field (field "a")
											(e-int @5.7-5.8 (raw "0"))))
									(e-ident @5.10-5.12 (raw "a")))
								(e-ident @5.15-5.18 (raw "age")))
							(e-field-access @5.21-5.31
								(e-record @5.21-5.29
									(field (field "a")
										(e-int @5.26-5.27 (raw "0"))))
								(e-ident @5.29-5.31 (raw "a"))))))))))
~~~
# FORMATTED
~~~roc
extract_age : { age : U64 } -> U64
extract_age = |person| {
	{ age } = person

	{ a: 0 }.a + age - { a: 0 }.a
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.12 (ident "extract_age"))
		(e-lambda @2.15-6.2
			(args
				(p-assign @2.16-2.22 (ident "person")))
			(e-block @2.24-6.2
				(s-let @3.5-3.21
					(p-record-destructure @3.5-3.12
						(destructs
							(record-destruct @3.7-3.10 (label "age") (ident "age")
								(required
									(p-assign @3.7-3.10 (ident "age"))))))
					(e-lookup-local @3.15-3.21
						(p-assign @2.16-2.22 (ident "person"))))
				(e-binop @5.2-5.31 (op "sub")
					(e-binop @5.2-5.18 (op "add")
						(e-dot-access @5.2-5.12 (field "a")
							(receiver
								(e-record @5.2-5.10
									(fields
										(field (name "a")
											(e-num @5.7-5.8 (value "0")))))))
						(e-lookup-local @5.15-5.18
							(p-assign @3.7-3.10 (ident "age"))))
					(e-dot-access @5.21-5.31 (field "a")
						(receiver
							(e-record @5.21-5.29
								(fields
									(field (name "a")
										(e-num @5.26-5.27 (value "0"))))))))))
		(annotation @2.1-2.12
			(declared-type
				(ty-fn @1.15-1.35 (effectful false)
					(ty-record @1.15-1.28
						(field (field "age")
							(ty-lookup @1.23-1.26 (name "U64") (builtin))))
					(ty-lookup @1.32-1.35 (name "U64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.12 (type "{ age: Num(Int(Unsigned64)) } -> Num(Int(Unsigned64))")))
	(expressions
		(expr @2.15-6.2 (type "{ age: Num(Int(Unsigned64)) } -> Num(Int(Unsigned64))"))))
~~~
