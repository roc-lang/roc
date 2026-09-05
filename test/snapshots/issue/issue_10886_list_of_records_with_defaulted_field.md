# META
~~~ini
description=Expected call argument types reach nested record constructions with defaulted fields
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/10886
Foo := { a : U64, b : U64 ?? 0 }
Wrapped : { items : List(Foo) }
Pair : (List(Foo), U64)
Choice : [Items(List(Foo))]
Nominal := { items : List(Foo) }

count : List(Foo) -> U64
count = |foos| List.len(foos)

count_wrapped : Wrapped -> U64
count_wrapped = |wrapped| List.len(wrapped.items)

count_pair : Pair -> U64
count_pair = |pair| List.len(pair.0)

count_choice : Choice -> U64
count_choice = |choice| match choice {
    Items(items) => List.len(items)
}

replace_items : Wrapped -> Wrapped
replace_items = |wrapped| {
    ..wrapped,
    items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}

direct = count([Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }])

nested_record = count_wrapped({
    items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
})

nested_tuple = count_pair((
    [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
    0,
))

nested_tag = count_choice(Items([
    Foo.{ a: 123 },
    Foo.{ a: 456, b: 789 },
]))

nested_branch = count(if True {
    [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }]
} else {
    [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }]
})

updated_record = replace_items({ items: [] })

nominal = Nominal.{
    items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,OpDoubleQuestion,Int,CloseCurly,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
UpperIdent,OpColon,OpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseRound,
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotInt,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
DoubleDot,LowerIdent,Comma,
LowerIdent,OpColon,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
CloseCurly,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,NoSpaceOpenRound,
OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
Int,Comma,
CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,OpenSquare,
UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,
UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,Comma,
CloseSquare,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,KwIf,UpperIdent,OpenCurly,
OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,
CloseCurly,KwElse,OpenCurly,
OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,
CloseCurly,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,CloseRound,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,Comma,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseSquare,Comma,
CloseCurly,
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
						(e-int (raw "0"))))))
		(s-type-decl
			(header (name "Wrapped")
				(args))
			(ty-record
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty (name "Foo"))))))
		(s-type-decl
			(header (name "Pair")
				(args))
			(ty-tuple
				(ty-apply
					(ty (name "List"))
					(ty (name "Foo")))
				(ty (name "U64"))))
		(s-type-decl
			(header (name "Choice")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Items"))
						(ty-apply
							(ty (name "List"))
							(ty (name "Foo")))))))
		(s-type-decl
			(header (name "Nominal")
				(args))
			(ty-record
				(anno-record-field (name "items")
					(ty-apply
						(ty (name "List"))
						(ty (name "Foo"))))))
		(s-type-anno (name "count")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "Foo")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "count"))
			(e-lambda
				(args
					(p-ident (raw "foos")))
				(e-apply
					(e-ident (raw "List.len"))
					(e-ident (raw "foos")))))
		(s-type-anno (name "count_wrapped")
			(ty-fn
				(ty (name "Wrapped"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "count_wrapped"))
			(e-lambda
				(args
					(p-ident (raw "wrapped")))
				(e-apply
					(e-ident (raw "List.len"))
					(e-field-access
						(receiver
							(e-ident (raw "wrapped")))
						(segment (mode "required") (field "items"))))))
		(s-type-anno (name "count_pair")
			(ty-fn
				(ty (name "Pair"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "count_pair"))
			(e-lambda
				(args
					(p-ident (raw "pair")))
				(e-apply
					(e-ident (raw "List.len"))
					(e-tuple-access
						(e-ident (raw "pair"))
						".0"))))
		(s-type-anno (name "count_choice")
			(ty-fn
				(ty (name "Choice"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "count_choice"))
			(e-lambda
				(args
					(p-ident (raw "choice")))
				(e-match
					(e-ident (raw "choice"))
					(branches
						(branch
							(p-tag (raw "Items")
								(p-ident (raw "items")))
							(e-apply
								(e-ident (raw "List.len"))
								(e-ident (raw "items"))))))))
		(s-type-anno (name "replace_items")
			(ty-fn
				(ty (name "Wrapped"))
				(ty (name "Wrapped"))))
		(s-decl
			(p-ident (raw "replace_items"))
			(e-lambda
				(args
					(p-ident (raw "wrapped")))
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
											(e-int (raw "789")))))))))))
		(s-decl
			(p-ident (raw "direct"))
			(e-apply
				(e-ident (raw "count"))
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
			(p-ident (raw "nested_record"))
			(e-apply
				(e-ident (raw "count_wrapped"))
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
			(p-ident (raw "nested_tuple"))
			(e-apply
				(e-ident (raw "count_pair"))
				(e-tuple
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
										(e-int (raw "789")))))))
					(e-int (raw "0")))))
		(s-decl
			(p-ident (raw "nested_tag"))
			(e-apply
				(e-ident (raw "count_choice"))
				(e-apply
					(e-tag (raw "Items"))
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
			(p-ident (raw "nested_branch"))
			(e-apply
				(e-ident (raw "count"))
				(e-if-then-else
					(e-tag (raw "True"))
					(e-block
						(statements
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
					(e-block
						(statements
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
		(s-decl
			(p-ident (raw "updated_record"))
			(e-apply
				(e-ident (raw "replace_items"))
				(e-record
					(field (field "items")
						(e-list)))))
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
												(e-int (raw "789"))))))))))))))
~~~
# FORMATTED
~~~roc
# repro for https://github.com/roc-lang/roc/issues/10886
Foo := { a : U64, b : U64 ?? 0 }

Wrapped : { items : List(Foo) }

Pair : (List(Foo), U64)

Choice : [Items(List(Foo))]

Nominal := { items : List(Foo) }

count : List(Foo) -> U64
count = |foos| List.len(foos)

count_wrapped : Wrapped -> U64
count_wrapped = |wrapped| List.len(wrapped.items)

count_pair : Pair -> U64
count_pair = |pair| List.len(pair.0)

count_choice : Choice -> U64
count_choice = |choice| match choice {
	Items(items) => List.len(items)
}

replace_items : Wrapped -> Wrapped
replace_items = |wrapped| {
	..wrapped,
	items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}

direct = count([Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }])

nested_record = count_wrapped({
	items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
})

nested_tuple = count_pair((
	[Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
	0,
))

nested_tag = count_choice(
	Items([
		Foo.{ a: 123 },
		Foo.{ a: 456, b: 789 },
	]),
)

nested_branch = count(
	if True {
		[Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }]
	} else {
		[Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }]
	},
)

updated_record = replace_items({ items: [] })

nominal = Nominal.{
	items: [Foo.{ a: 123 }, Foo.{ a: 456, b: 789 }],
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "count"))
		(e-lambda
			(args
				(p-assign (ident "foos")))
			(e-call (constraint-fn-var 517)
				(e-lookup-external
					(builtin))
				(e-lookup-local
					(p-assign (ident "foos")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local)))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "count_wrapped"))
		(e-lambda
			(args
				(p-assign (ident "wrapped")))
			(e-call (constraint-fn-var 537)
				(e-lookup-external
					(builtin))
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "wrapped"))))
					(segments
						(segment (name "items") (mode "required"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Wrapped") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "count_pair"))
		(e-lambda
			(args
				(p-assign (ident "pair")))
			(e-call (constraint-fn-var 555)
				(e-lookup-external
					(builtin))
				(e-tuple-access (index "0")
					(e-lookup-local
						(p-assign (ident "pair"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Pair") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "count_choice"))
		(e-lambda
			(args
				(p-assign (ident "choice")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "choice"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-call (constraint-fn-var 575)
									(e-lookup-external
										(builtin))
									(e-lookup-local
										(p-assign (ident "items"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Choice") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "replace_items"))
		(e-lambda
			(args
				(p-assign (ident "wrapped")))
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
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Wrapped") (local))
				(ty-lookup (name "Wrapped") (local)))))
	(d-let
		(p-assign (ident "direct"))
		(e-call (constraint-fn-var 758)
			(e-lookup-local
				(p-assign (ident "count")))
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
		(p-assign (ident "nested_record"))
		(e-call (constraint-fn-var 849)
			(e-lookup-local
				(p-assign (ident "count_wrapped")))
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
		(p-assign (ident "nested_tuple"))
		(e-call (constraint-fn-var 942)
			(e-lookup-local
				(p-assign (ident "count_pair")))
			(e-tuple
				(elems
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
											(e-num (value "789"))))))))
					(e-num (value "0"))))))
	(d-let
		(p-assign (ident "nested_tag"))
		(e-call (constraint-fn-var 1029)
			(e-lookup-local
				(p-assign (ident "count_choice")))
			(e-tag (name "Items")
				(args
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
		(p-assign (ident "nested_branch"))
		(e-call (constraint-fn-var 1179)
			(e-lookup-local
				(p-assign (ident "count")))
			(e-if
				(if-branches
					(if-branch
						(e-tag (name "True"))
						(e-block
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
													(e-num (value "789")))))))))))
				(if-else
					(e-block
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
		(p-assign (ident "updated_record"))
		(e-call (constraint-fn-var 1193)
			(e-lookup-local
				(p-assign (ident "replace_items")))
			(e-record
				(fields
					(field (name "items")
						(e-empty_list))))))
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
	(s-alias-decl
		(ty-header (name "Pair"))
		(ty-tuple
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Foo") (local)))
			(ty-lookup (name "U64") (builtin))))
	(s-alias-decl
		(ty-header (name "Choice"))
		(ty-tag-union
			(ty-tag-name (name "Items")
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local))))))
	(s-nominal-decl
		(ty-header (name "Nominal"))
		(ty-record
			(field (field "items")
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Foo") (local)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Foo) -> U64"))
		(patt (type "Wrapped -> U64"))
		(patt (type "Pair -> U64"))
		(patt (type "Choice -> U64"))
		(patt (type "Wrapped -> Wrapped"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "U64"))
		(patt (type "Wrapped"))
		(patt (type "Nominal")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Wrapped")
			(ty-header (name "Wrapped")))
		(alias (type "Pair")
			(ty-header (name "Pair")))
		(alias (type "Choice")
			(ty-header (name "Choice")))
		(nominal (type "Nominal")
			(ty-header (name "Nominal"))))
	(expressions
		(expr (type "List(Foo) -> U64"))
		(expr (type "Wrapped -> U64"))
		(expr (type "Pair -> U64"))
		(expr (type "Choice -> U64"))
		(expr (type "Wrapped -> Wrapped"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "U64"))
		(expr (type "Wrapped"))
		(expr (type "Nominal"))))
~~~
