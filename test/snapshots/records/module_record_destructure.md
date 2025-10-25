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
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
OpenCurly,LowerIdent,OpColon,Int,CloseCurly,NoSpaceDotLowerIdent,OpPlus,LowerIdent,OpBinaryMinus,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,NoSpaceDotLowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "extract_age")
			(ty-fn
				(ty-record
					(anno-record-field (name "age")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "extract_age"))
			(e-lambda
				(args
					(p-ident (raw "person")))
				(e-block
					(statements
						(s-decl
							(p-record
								(field (name "age") (rest false)))
							(e-ident (raw "person")))
						(e-binop (op "-")
							(e-binop (op "+")
								(e-field-access
									(e-record
										(field (field "a")
											(e-int (raw "0"))))
									(e-ident (raw "a")))
								(e-ident (raw "age")))
							(e-field-access
								(e-record
									(field (field "a")
										(e-int (raw "0"))))
								(e-ident (raw "a"))))))))))
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
		(p-assign (ident "extract_age"))
		(e-lambda
			(args
				(p-assign (ident "person")))
			(e-block
				(s-let
					(p-record-destructure
						(destructs
							(record-destruct (label "age") (ident "age")
								(required
									(p-assign (ident "age"))))))
					(e-lookup-local
						(p-assign (ident "person"))))
				(e-binop (op "sub")
					(e-binop (op "add")
						(e-dot-access (field "a")
							(receiver
								(e-record
									(fields
										(field (name "a")
											(e-num (value "0")))))))
						(e-lookup-local
							(p-assign (ident "age"))))
					(e-dot-access (field "a")
						(receiver
							(e-record
								(fields
									(field (name "a")
										(e-num (value "0"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "age")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ age: Num(Int(Unsigned64)) } -> Num(Int(Unsigned64))")))
	(expressions
		(expr (type "{ age: Num(Int(Unsigned64)) } -> Num(Int(Unsigned64))"))))
~~~
