# META
~~~ini
description=Call-expected list records materialize omitted defaulted fields at runtime
type=snippet
~~~
# SOURCE
~~~roc
Foo := { a : U64, b : U64 ?? 10 }
Wrapped : { items : List(Foo) }
Nominal := { items : List(Foo) }

accept : List(Foo) -> List(Foo)
accept = |foos| foos

accept_wrapped : Wrapped -> Wrapped
accept_wrapped = |wrapped| wrapped

foos = accept([Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }])
wrapped = accept_wrapped({
    items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
})
updated = {
    ..wrapped,
    items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}
nominal = Nominal.{
    items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}

expect List.fold(foos, 0, |sum, foo| sum + foo.b) == 799
expect List.fold(wrapped.items, 0, |sum, foo| sum + foo.b) == 799
expect List.fold(updated.items, 0, |sum, foo| sum + foo.b) == 799
expect List.fold(nominal.items, 0, |sum, foo| sum + foo.b) == 799
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
CloseCurly,CloseRound,
LowerIdent,OpAssign,OpenCurly,
DoubleDot,LowerIdent,Comma,
LowerIdent,OpColon,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
CloseCurly,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
CloseCurly,
KwExpect,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,CloseRound,OpEquals,Int,
KwExpect,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,CloseRound,OpEquals,Int,
KwExpect,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,CloseRound,OpEquals,Int,
KwExpect,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,Comma,Int,Comma,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,CloseRound,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-record
				(anno-record-field (name "a")
					(ty (name "U64")))
				(anno-record-field (name "b")
					(ty (name "U64"))
					(default
						(e-int (raw "10"))))))
		(s-type-decl
			(header (name "Wrapped")
				(args))
			(ty-record
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty (name "Foo"))))))
		(s-type-decl
			(header (name "Nominal")
				(args))
			(ty-record
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty (name "Foo"))))))
		(s-type-anno (name "accept")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "Foo")))
				(ty-apply
					(ty (name "List"))
					(ty (name "Foo")))))
		(s-decl
			(p-ident (raw "accept"))
			(e-lambda
				(args
					(p-ident (raw "foos")))
				(e-ident (raw "foos"))))
		(s-type-anno (name "accept_wrapped")
			(ty-fn
				(ty (name "Wrapped"))
				(ty (name "Wrapped"))))
		(s-decl
			(p-ident (raw "accept_wrapped"))
			(e-lambda
				(args
					(p-ident (raw "wrapped")))
				(e-ident (raw "wrapped"))))
		(s-decl
			(p-ident (raw "foos"))
			(e-apply
				(e-ident (raw "accept"))
				(e-list
					(e-nominal-record
						(mapper (e-tag (raw "Foo")))
						(backing (e-record
								(field (field "a")
									(e-int (raw "123"))))))
					(e-nominal-record
						(mapper (e-tag (raw "Foo")))
						(backing (e-record
								(field (field "a")
									(e-int (raw "456")))
								(field (field "b")
									(e-int (raw "789")))))))))
		(s-decl
			(p-ident (raw "wrapped"))
			(e-apply
				(e-ident (raw "accept_wrapped"))
				(e-record
					(field (field "items")
						(e-list
							(e-nominal-record
								(mapper (e-tag (raw "Foo")))
								(backing (e-record
										(field (field "a")
											(e-int (raw "123"))))))
							(e-nominal-record
								(mapper (e-tag (raw "Foo")))
								(backing (e-record
										(field (field "a")
											(e-int (raw "456")))
										(field (field "b")
											(e-int (raw "789")))))))))))
		(s-decl
			(p-ident (raw "updated"))
			(e-record
				(ext
					(e-ident (raw "wrapped")))
				(field (field "items")
					(e-list
						(e-nominal-record
							(mapper (e-tag (raw "Foo")))
							(backing (e-record
									(field (field "a")
										(e-int (raw "123"))))))
						(e-nominal-record
							(mapper (e-tag (raw "Foo")))
							(backing (e-record
									(field (field "a")
										(e-int (raw "456")))
									(field (field "b")
										(e-int (raw "789"))))))))))
		(s-decl
			(p-ident (raw "nominal"))
			(e-nominal-record
				(mapper (e-tag (raw "Nominal")))
				(backing (e-record
						(field (field "items")
							(e-list
								(e-nominal-record
									(mapper (e-tag (raw "Foo")))
									(backing (e-record
											(field (field "a")
												(e-int (raw "123"))))))
								(e-nominal-record
									(mapper (e-tag (raw "Foo")))
									(backing (e-record
											(field (field "a")
												(e-int (raw "456")))
											(field (field "b")
												(e-int (raw "789"))))))))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "List.fold"))
					(e-ident (raw "foos"))
					(e-int (raw "0"))
					(e-lambda
						(args
							(p-ident (raw "sum"))
							(p-ident (raw "foo")))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-field-access
								(receiver
									(e-ident (raw "foo")))
								(segment (mode "required") (field "b"))))))
				(e-int (raw "799"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "List.fold"))
					(e-field-access
						(receiver
							(e-ident (raw "wrapped")))
						(segment (mode "required") (field "items")))
					(e-int (raw "0"))
					(e-lambda
						(args
							(p-ident (raw "sum"))
							(p-ident (raw "foo")))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-field-access
								(receiver
									(e-ident (raw "foo")))
								(segment (mode "required") (field "b"))))))
				(e-int (raw "799"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "List.fold"))
					(e-field-access
						(receiver
							(e-ident (raw "updated")))
						(segment (mode "required") (field "items")))
					(e-int (raw "0"))
					(e-lambda
						(args
							(p-ident (raw "sum"))
							(p-ident (raw "foo")))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-field-access
								(receiver
									(e-ident (raw "foo")))
								(segment (mode "required") (field "b"))))))
				(e-int (raw "799"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "List.fold"))
					(e-field-access
						(receiver
							(e-ident (raw "nominal")))
						(segment (mode "required") (field "items")))
					(e-int (raw "0"))
					(e-lambda
						(args
							(p-ident (raw "sum"))
							(p-ident (raw "foo")))
						(e-binop (op "+")
							(e-ident (raw "sum"))
							(e-field-access
								(receiver
									(e-ident (raw "foo")))
								(segment (mode "required") (field "b"))))))
				(e-int (raw "799"))))))
~~~
# FORMATTED
~~~roc
Foo := { a : U64, b : U64 ?? 10 }

Wrapped : { items : List(Foo) }

Nominal := { items : List(Foo) }

accept : List(Foo) -> List(Foo)
accept = |foos| foos

accept_wrapped : Wrapped -> Wrapped
accept_wrapped = |wrapped| wrapped

foos = accept([Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }])

wrapped = accept_wrapped({
	items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
})

updated = {
	..wrapped,
	items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}

nominal = Nominal.{
	items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}

expect List.fold(foos, 0, |sum, foo| sum + foo.b) == 799
expect List.fold(wrapped.items, 0, |sum, foo| sum + foo.b) == 799
expect List.fold(updated.items, 0, |sum, foo| sum + foo.b) == 799
expect List.fold(nominal.items, 0, |sum, foo| sum + foo.b) == 799
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "accept"))
		(e-lambda
			(args
				(p-assign (ident "foos")))
			(e-lookup-local
				(p-assign (ident "foos"))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local)))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local))))))
	(d-let
		(p-assign (ident "accept_wrapped"))
		(e-lambda
			(args
				(p-assign (ident "wrapped")))
			(e-lookup-local
				(p-assign (ident "wrapped"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Wrapped") (local))
				(ty-lookup (name "Wrapped") (local)))))
	(d-let
		(p-assign (ident "foos"))
		(e-call (constraint-fn-var 511)
			(e-lookup-local
				(p-assign (ident "accept")))
			(e-list
				(elems
					(e-nominal (nominal "Foo")
						(e-record
							(fields
								(field (name "a")
									(e-num (value "123"))))))
					(e-nominal (nominal "Foo")
						(e-record
							(fields
								(field (name "a")
									(e-num (value "456")))
								(field (name "b")
									(e-num (value "789"))))))))))
	(d-let
		(p-assign (ident "wrapped"))
		(e-call (constraint-fn-var 601)
			(e-lookup-local
				(p-assign (ident "accept_wrapped")))
			(e-record
				(fields
					(field (name "items")
						(e-list
							(elems
								(e-nominal (nominal "Foo")
									(e-record
										(fields
											(field (name "a")
												(e-num (value "123"))))))
								(e-nominal (nominal "Foo")
									(e-record
										(fields
											(field (name "a")
												(e-num (value "456")))
											(field (name "b")
												(e-num (value "789")))))))))))))
	(d-let
		(p-assign (ident "updated"))
		(e-record
			(ext
				(e-lookup-local
					(p-assign (ident "wrapped"))))
			(fields
				(field (name "items")
					(e-list
						(elems
							(e-nominal (nominal "Foo")
								(e-record
									(fields
										(field (name "a")
											(e-num (value "123"))))))
							(e-nominal (nominal "Foo")
								(e-record
									(fields
										(field (name "a")
											(e-num (value "456")))
										(field (name "b")
											(e-num (value "789"))))))))))))
	(d-let
		(p-assign (ident "nominal"))
		(e-nominal (nominal "Nominal")
			(e-record
				(fields
					(field (name "items")
						(e-list
							(elems
								(e-nominal (nominal "Foo")
									(e-record
										(fields
											(field (name "a")
												(e-num (value "123"))))))
								(e-nominal (nominal "Foo")
									(e-record
										(fields
											(field (name "a")
												(e-num (value "456")))
											(field (name "b")
												(e-num (value "789")))))))))))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-record
			(field (field "a")
				(ty-lookup (name "U64") (builtin)))
			(field (field "b") (defaulted true)
				(ty-lookup (name "U64") (builtin)))))
	(s-alias-decl
		(ty-header (name "Wrapped"))
		(ty-record
			(field (field "items")
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local))))))
	(s-nominal-decl
		(ty-header (name "Nominal"))
		(ty-record
			(field (field "items")
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 803)
					(e-lookup-external
						(builtin))
					(e-lookup-local
						(p-assign (ident "foos")))
					(e-num (value "0"))
					(e-lambda
						(args
							(p-assign (ident "sum"))
							(p-assign (ident "foo")))
						(e-dispatch-call (method "plus") (constraint-fn-var 794)
							(receiver
								(e-lookup-local
									(p-assign (ident "sum"))))
							(args
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "foo"))))
									(segments
										(segment (name "b") (mode "required")))))))))
			(rhs
				(e-num (value "799")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 838)
					(e-lookup-external
						(builtin))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "wrapped"))))
						(segments
							(segment (name "items") (mode "required"))))
					(e-num (value "0"))
					(e-lambda
						(args
							(p-assign (ident "sum"))
							(p-assign (ident "foo")))
						(e-dispatch-call (method "plus") (constraint-fn-var 834)
							(receiver
								(e-lookup-local
									(p-assign (ident "sum"))))
							(args
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "foo"))))
									(segments
										(segment (name "b") (mode "required")))))))))
			(rhs
				(e-num (value "799")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 873)
					(e-lookup-external
						(builtin))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "updated"))))
						(segments
							(segment (name "items") (mode "required"))))
					(e-num (value "0"))
					(e-lambda
						(args
							(p-assign (ident "sum"))
							(p-assign (ident "foo")))
						(e-dispatch-call (method "plus") (constraint-fn-var 869)
							(receiver
								(e-lookup-local
									(p-assign (ident "sum"))))
							(args
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "foo"))))
									(segments
										(segment (name "b") (mode "required")))))))))
			(rhs
				(e-num (value "799")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 912)
					(e-lookup-external
						(builtin))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "nominal"))))
						(segments
							(segment (name "items") (mode "required"))))
					(e-num (value "0"))
					(e-lambda
						(args
							(p-assign (ident "sum"))
							(p-assign (ident "foo")))
						(e-dispatch-call (method "plus") (constraint-fn-var 908)
							(receiver
								(e-lookup-local
									(p-assign (ident "sum"))))
							(args
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "foo"))))
									(segments
										(segment (name "b") (mode "required")))))))))
			(rhs
				(e-num (value "799"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Foo) -> List(Foo)"))
		(patt (type "Wrapped -> Wrapped"))
		(patt (type "List(Foo)"))
		(patt (type "Wrapped"))
		(patt (type "Wrapped"))
		(patt (type "Nominal")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Wrapped")
			(ty-header (name "Wrapped")))
		(nominal (type "Nominal")
			(ty-header (name "Nominal"))))
	(expressions
		(expr (type "List(Foo) -> List(Foo)"))
		(expr (type "Wrapped -> Wrapped"))
		(expr (type "List(Foo)"))
		(expr (type "Wrapped"))
		(expr (type "Wrapped"))
		(expr (type "Nominal"))))
~~~
