# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module [extract_age]

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:20),CloseSquare(1:20-1:21),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),OpenCurly(3:15-3:16),LowerIdent(3:17-3:20),OpColon(3:21-3:22),UpperIdent(3:23-3:26),CloseCurly(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:35),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpBar(4:15-4:16),LowerIdent(4:16-4:22),OpBar(4:22-4:23),OpenCurly(4:24-4:25),
OpenCurly(5:5-5:6),LowerIdent(5:7-5:10),CloseCurly(5:11-5:12),OpAssign(5:13-5:14),LowerIdent(5:15-5:21),
OpenCurly(7:2-7:3),LowerIdent(7:4-7:5),OpColon(7:5-7:6),Int(7:7-7:8),CloseCurly(7:9-7:10),NoSpaceDotLowerIdent(7:10-7:12),OpPlus(7:13-7:14),LowerIdent(7:15-7:18),OpBinaryMinus(7:19-7:20),OpenCurly(7:21-7:22),LowerIdent(7:23-7:24),OpColon(7:24-7:25),Int(7:26-7:27),CloseCurly(7:28-7:29),NoSpaceDotLowerIdent(7:29-7:31),
CloseCurly(8:1-8:2),EndOfFile(8:2-8:2),
~~~
# PARSE
~~~clojure
(file @1.1-8.2
	(module @1.1-1.21
		(exposes @1.8-1.21
			(exposed-lower-ident @1.9-1.20
				(text "extract_age"))))
	(statements
		(s-type-anno @3.1-3.35 (name "extract_age")
			(ty-fn @3.15-3.35
				(ty-record @3.15-3.28
					(anno-record-field @3.17-3.26 (name "age")
						(ty @3.23-3.26 (name "U64"))))
				(ty @3.32-3.35 (name "U64"))))
		(s-decl @4.1-8.2
			(p-ident @4.1-4.12 (raw "extract_age"))
			(e-lambda @4.15-8.2
				(args
					(p-ident @4.16-4.22 (raw "person")))
				(e-block @4.24-8.2
					(statements
						(s-decl @5.5-5.21
							(p-record @5.5-5.12
								(field @5.7-5.10 (name "age") (rest false)))
							(e-ident @5.15-5.21 (raw "person")))
						(e-binop @7.2-7.31 (op "-")
							(e-binop @7.2-7.18 (op "+")
								(e-field-access @7.2-7.12
									(e-record @7.2-7.10
										(field (field "a")
											(e-int @7.7-7.8 (raw "0"))))
									(e-ident @7.10-7.12 (raw "a")))
								(e-ident @7.15-7.18 (raw "age")))
							(e-field-access @7.21-7.31
								(e-record @7.21-7.29
									(field (field "a")
										(e-int @7.26-7.27 (raw "0"))))
								(e-ident @7.29-7.31 (raw "a"))))))))))
~~~
# FORMATTED
~~~roc
module [extract_age]

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
		(p-assign @4.1-4.12 (ident "extract_age"))
		(e-lambda @4.15-8.2
			(args
				(p-assign @4.16-4.22 (ident "person")))
			(e-block @4.24-8.2
				(s-let @5.5-5.21
					(p-record-destructure @5.5-5.12
						(destructs
							(record-destruct @5.7-5.10 (label "age") (ident "age")
								(required
									(p-assign @5.7-5.10 (ident "age"))))))
					(e-lookup-local @5.15-5.21
						(p-assign @4.16-4.22 (ident "person"))))
				(e-binop @7.2-7.31 (op "sub")
					(e-binop @7.2-7.18 (op "add")
						(e-dot-access @7.2-7.12 (field "a")
							(receiver
								(e-record @7.2-7.10
									(fields
										(field (name "a")
											(e-int @7.7-7.8 (value "0")))))))
						(e-lookup-local @7.15-7.18
							(p-assign @5.7-5.10 (ident "age"))))
					(e-dot-access @7.21-7.31 (field "a")
						(receiver
							(e-record @7.21-7.29
								(fields
									(field (name "a")
										(e-int @7.26-7.27 (value "0"))))))))))
		(annotation @4.1-4.12
			(declared-type
				(ty-fn @3.15-3.35 (effectful false)
					(ty-record @3.15-3.28
						(field (field "age")
							(ty @3.23-3.26 (name "u64"))))
					(ty @3.32-3.35 (name "u64")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.12 (type "{ age: U64 } -> U64")))
	(expressions
		(expr @4.15-8.2 (type "{ age: U64 } -> U64"))))
~~~
