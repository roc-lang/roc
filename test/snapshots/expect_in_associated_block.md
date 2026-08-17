# META
~~~ini
description=An expect inside a nominal type's associated block is a top-level expect (issue #10730)
type=snippet
~~~
# SOURCE
~~~roc
Counter := { count : U64 }.{
	bump : Counter -> Counter
	bump = |Counter.({ count })| Counter.({ count: count + 1 })

	count_of : Counter -> U64
	count_of = |Counter.(inner)| inner.count

	expect Counter.({ count: 1 }).bump().count_of() == 2

	Flag := [On, Off].{
		toggle : Flag -> Flag
		toggle = |Flag.(state)| match state {
			On => Flag.(Off)
			Off => Flag.(On)
		}

		to_num : Flag -> U64
		to_num = |Flag.(state)| match state {
			On => 1
			Off => 0
		}

		expect Flag.(Off).toggle().to_num() == 1
	}
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,Dot,NoSpaceOpenRound,OpenCurly,LowerIdent,CloseCurly,CloseRound,OpBar,UpperIdent,Dot,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,OpPlus,Int,CloseCurly,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,NoSpaceDotLowerIdent,
KwExpect,UpperIdent,Dot,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,CloseCurly,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,Int,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,Dot,NoSpaceOpenRound,UpperIdent,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,Dot,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,Dot,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
CloseCurly,
KwExpect,UpperIdent,Dot,NoSpaceOpenRound,UpperIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpEquals,Int,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Counter")
				(args))
			(ty-record
				(anno-record-field (name "count")
					(ty (name "U64"))))
			(associated
				(s-type-anno (name "bump")
					(ty-fn
						(ty (name "Counter"))
						(ty (name "Counter"))))
				(s-decl
					(p-ident (raw "bump"))
					(e-lambda
						(args
							(p-tag (raw "Counter")
								(p-record
									(field (name "count") (rest false)))))
						(e-nominal-apply
							(mapper (e-tag (raw "Counter")))
							(e-record
								(field (field "count")
									(e-binop (op "+")
										(e-ident (raw "count"))
										(e-int (raw "1"))))))))
				(s-type-anno (name "count_of")
					(ty-fn
						(ty (name "Counter"))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "count_of"))
					(e-lambda
						(args
							(p-tag (raw "Counter")
								(p-ident (raw "inner"))))
						(e-field-access
							(receiver
								(e-ident (raw "inner")))
							(segment (mode "required") (field "count")))))
				(s-expect
					(e-binop (op "==")
						(e-method-call (method ".count_of")
							(receiver
								(e-method-call (method ".bump")
									(receiver
										(e-nominal-apply
											(mapper (e-tag (raw "Counter")))
											(e-record
												(field (field "count")
													(e-int (raw "1"))))))
									(args)))
							(args))
						(e-int (raw "2"))))
				(s-type-decl
					(header (name "Flag")
						(args))
					(ty-tag-union
						(tags
							(ty (name "On"))
							(ty (name "Off"))))
					(associated
						(s-type-anno (name "toggle")
							(ty-fn
								(ty (name "Flag"))
								(ty (name "Flag"))))
						(s-decl
							(p-ident (raw "toggle"))
							(e-lambda
								(args
									(p-tag (raw "Flag")
										(p-ident (raw "state"))))
								(e-match
									(e-ident (raw "state"))
									(branches
										(branch
											(p-tag (raw "On"))
											(e-nominal-apply
												(mapper (e-tag (raw "Flag")))
												(e-tag (raw "Off"))))
										(branch
											(p-tag (raw "Off"))
											(e-nominal-apply
												(mapper (e-tag (raw "Flag")))
												(e-tag (raw "On"))))))))
						(s-type-anno (name "to_num")
							(ty-fn
								(ty (name "Flag"))
								(ty (name "U64"))))
						(s-decl
							(p-ident (raw "to_num"))
							(e-lambda
								(args
									(p-tag (raw "Flag")
										(p-ident (raw "state"))))
								(e-match
									(e-ident (raw "state"))
									(branches
										(branch
											(p-tag (raw "On"))
											(e-int (raw "1")))
										(branch
											(p-tag (raw "Off"))
											(e-int (raw "0")))))))
						(s-expect
							(e-binop (op "==")
								(e-method-call (method ".to_num")
									(receiver
										(e-method-call (method ".toggle")
											(receiver
												(e-nominal-apply
													(mapper (e-tag (raw "Flag")))
													(e-tag (raw "Off"))))
											(args)))
									(args))
								(e-int (raw "1"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "expect_in_associated_block.Counter.bump"))
		(e-lambda
			(args
				(p-nominal
					(p-record-destructure
						(destructs
							(record-destruct (label "count") (ident "count")
								(required
									(p-assign (ident "count"))))))))
			(e-nominal (nominal "Counter")
				(e-record
					(fields
						(field (name "count")
							(e-dispatch-call (method "plus") (constraint-fn-var 337)
								(receiver
									(e-lookup-local
										(p-assign (ident "count"))))
								(args
									(e-num (value "1")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Counter") (local))
				(ty-lookup (name "Counter") (local)))))
	(d-let
		(p-assign (ident "expect_in_associated_block.Counter.count_of"))
		(e-lambda
			(args
				(p-nominal
					(p-assign (ident "inner"))))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "inner"))))
				(segments
					(segment (name "count") (mode "required")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Counter") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "expect_in_associated_block.Counter.Flag.toggle"))
		(e-lambda
			(args
				(p-nominal
					(p-assign (ident "state"))))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "state"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal (nominal "expect_in_associated_block.Counter.Flag")
									(e-tag (name "Off")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal (nominal "expect_in_associated_block.Counter.Flag")
									(e-tag (name "On")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Flag") (local))
				(ty-lookup (name "Flag") (local)))))
	(d-let
		(p-assign (ident "expect_in_associated_block.Counter.Flag.to_num"))
		(e-lambda
			(args
				(p-nominal
					(p-assign (ident "state"))))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "state"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "1"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Flag") (local))
				(ty-lookup (name "U64") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Counter"))
		(ty-record
			(field (field "count")
				(ty-lookup (name "U64") (builtin)))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-dispatch-call (method "count_of") (constraint-fn-var 430)
					(receiver
						(e-dispatch-call (method "bump") (constraint-fn-var 428)
							(receiver
								(e-nominal (nominal "Counter")
									(e-record
										(fields
											(field (name "count")
												(e-num (value "1")))))))
							(args)))
					(args)))
			(rhs
				(e-num (value "2")))))
	(s-nominal-decl
		(ty-header (name "expect_in_associated_block.Counter.Flag"))
		(ty-tag-union
			(ty-tag-name (name "On"))
			(ty-tag-name (name "Off"))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-dispatch-call (method "to_num") (constraint-fn-var 456)
					(receiver
						(e-dispatch-call (method "toggle") (constraint-fn-var 454)
							(receiver
								(e-nominal (nominal "expect_in_associated_block.Counter.Flag")
									(e-tag (name "Off"))))
							(args)))
					(args)))
			(rhs
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Counter -> Counter"))
		(patt (type "Counter -> U64"))
		(patt (type "Counter.Flag -> Counter.Flag"))
		(patt (type "Counter.Flag -> U64")))
	(type_decls
		(nominal (type "Counter")
			(ty-header (name "Counter")))
		(nominal (type "Counter.Flag")
			(ty-header (name "expect_in_associated_block.Counter.Flag"))))
	(expressions
		(expr (type "Counter -> Counter"))
		(expr (type "Counter -> U64"))
		(expr (type "Counter.Flag -> Counter.Flag"))
		(expr (type "Counter.Flag -> U64"))))
~~~
