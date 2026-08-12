# META
~~~ini
description=Destructuring an optional field binds a flat Try the binder can return, default, match, or reach through with a nested Ok/Err pattern
type=snippet
~~~
# SOURCE
~~~roc
Rec : { x ?: U8 }

as_try : Rec -> Try(U8, [MissingField])
as_try = |rec| {
    { x } = rec
    x
}

defaulted : Rec -> U8
defaulted = |rec| {
    { x } = rec
    x ?? 0
}

classify : Rec -> U8
classify = |rec| {
    { x } = rec
    match x {
        Ok(v) => v
        Err(MissingField) => 0
    }
}

nested : Rec -> U8
nested = |rec| match rec {
    { x: Ok(y) } => y
    { x: Err(_) } => 0
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpQuestion,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
LowerIdent,OpDoubleQuestion,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,Int,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,OpFatArrow,LowerIdent,
OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,CloseCurly,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Rec")
				(args))
			(ty-record
				(anno-record-field (name "x") (optional true)
					(ty (name "U8")))))
		(s-type-anno (name "as_try")
			(ty-fn
				(ty (name "Rec"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U8"))
					(ty-tag-union
						(tags
							(ty (name "MissingField")))))))
		(s-decl
			(p-ident (raw "as_try"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-block
					(statements
						(s-decl
							(p-record
								(field (name "x") (rest false)))
							(e-ident (raw "rec")))
						(e-ident (raw "x"))))))
		(s-type-anno (name "defaulted")
			(ty-fn
				(ty (name "Rec"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "defaulted"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-block
					(statements
						(s-decl
							(p-record
								(field (name "x") (rest false)))
							(e-ident (raw "rec")))
						(e-binop (op "??")
							(e-ident (raw "x"))
							(e-int (raw "0")))))))
		(s-type-anno (name "classify")
			(ty-fn
				(ty (name "Rec"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "classify"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-block
					(statements
						(s-decl
							(p-record
								(field (name "x") (rest false)))
							(e-ident (raw "rec")))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "Ok")
										(p-ident (raw "v")))
									(e-ident (raw "v")))
								(branch
									(p-tag (raw "Err")
										(p-tag (raw "MissingField")))
									(e-int (raw "0")))))))))
		(s-type-anno (name "nested")
			(ty-fn
				(ty (name "Rec"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "nested"))
			(e-lambda
				(args
					(p-ident (raw "rec")))
				(e-match
					(e-ident (raw "rec"))
					(branches
						(branch
							(p-record
								(field (name "x") (rest false)
									(p-tag (raw "Ok")
										(p-ident (raw "y")))))
							(e-ident (raw "y")))
						(branch
							(p-record
								(field (name "x") (rest false)
									(p-tag (raw "Err")
										(p-underscore))))
							(e-int (raw "0")))))))))
~~~
# FORMATTED
~~~roc
Rec : { x ?: U8 }

as_try : Rec -> Try(U8, [MissingField])
as_try = |rec| {
	{ x } = rec
	x
}

defaulted : Rec -> U8
defaulted = |rec| {
	{ x } = rec
	x ?? 0
}

classify : Rec -> U8
classify = |rec| {
	{ x } = rec
	match x {
		Ok(v) => v
		Err(MissingField) => 0
	}
}

nested : Rec -> U8
nested = |rec| match rec {
	{ x: Ok(y) } => y
	{ x: Err(_) } => 0
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "as_try"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-block
				(s-let
					(p-record-destructure
						(destructs
							(record-destruct (label "x") (ident "x")
								(required
									(p-assign (ident "x"))))))
					(e-lookup-local
						(p-assign (ident "rec"))))
				(e-lookup-local
					(p-assign (ident "x")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Rec") (local))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-tag-union
						(ty-tag-name (name "MissingField")))))))
	(d-let
		(p-assign (ident "defaulted"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-block
				(s-let
					(p-record-destructure
						(destructs
							(record-destruct (label "x") (ident "x")
								(required
									(p-assign (ident "x"))))))
					(e-lookup-local
						(p-assign (ident "rec"))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal-external (builtin)
											(p-applied-tag))))
								(value
									(e-lookup-local
										(p-assign (ident "#ok")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal-external (builtin)
											(p-applied-tag))))
								(value
									(e-num (value "0")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Rec") (local))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "classify"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-block
				(s-let
					(p-record-destructure
						(destructs
							(record-destruct (label "x") (ident "x")
								(required
									(p-assign (ident "x"))))))
					(e-lookup-local
						(p-assign (ident "rec"))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "x"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "v")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-num (value "0")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Rec") (local))
				(ty-lookup (name "U8") (builtin)))))
	(d-let
		(p-assign (ident "nested"))
		(e-lambda
			(args
				(p-assign (ident "rec")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "rec"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-record-destructure
										(destructs
											(record-destruct (label "x") (ident "x")
												(sub-pattern
													(p-applied-tag)))))))
							(value
								(e-lookup-local
									(p-assign (ident "y")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-record-destructure
										(destructs
											(record-destruct (label "x") (ident "x")
												(sub-pattern
													(p-applied-tag)))))))
							(value
								(e-num (value "0"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Rec") (local))
				(ty-lookup (name "U8") (builtin)))))
	(s-alias-decl
		(ty-header (name "Rec"))
		(ty-record
			(field (field "x") (optional true)
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Rec -> Try(U8, [MissingField])"))
		(patt (type "Rec -> U8"))
		(patt (type "Rec -> U8"))
		(patt (type "Rec -> U8")))
	(type_decls
		(alias (type "Rec")
			(ty-header (name "Rec"))))
	(expressions
		(expr (type "Rec -> Try(U8, [MissingField])"))
		(expr (type "Rec -> U8"))
		(expr (type "Rec -> U8"))
		(expr (type "Rec -> U8"))))
~~~
