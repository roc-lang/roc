# META
~~~ini
description=A method call's argument that conflicts with the value the method is called on is reported as a conflict between the two
type=file
~~~
# SOURCE
~~~roc
In(c) := [In(c)]

Val(ctx, a) := [Val(a)]

Handle(ctx, a) := [Handle(a)].{
	lift : Handle(c, a) -> Handle(In(c), a)
	lift = |Handle(a)| Handle(a)

	count : Handle(ctx, a), Val(ctx, List(b)) -> U64
	count = |_handle, Val(rows)| rows.len()

	pair : Handle(ctx, a), Val(ctx, b), Val(ctx, b) -> U64
	pair = |_handle, _x, _y| 0
}

total : Handle(c, U64), Val(In(c), List(Str)) -> U64
total = |handle, rows| handle.lift().lift().count(rows)

between_arguments : Handle(c, U64), Val(c, Str), Val(c, U8) -> U64
between_arguments = |handle, x, y| handle.pair(x, y)
~~~
# EXPECTED
TYPE MISMATCH - call_args_conflict_method_receiver.md:17:24:17:44
TYPE MISMATCH - call_args_conflict_method_receiver.md:20:48:20:49
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 17 24) (end 17 44))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "value this method is called on")
			(reflow " ")
			(reflow "and the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "count")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_method_receiver.md") (start 17 24) (end 17 44) (annotation error) (line-text "total = |handle, rows| handle.lift().lift().count(rows)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "value this method is called on")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Handle(In(In(c)), U64)")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_method_receiver.md") (start 17 51) (end 17 55) (annotation error) (line-text "total = |handle, rows| handle.lift().lift().count(rows)"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Val(In(c), List(Str))")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "count")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Handle(ctx, a), Val(ctx, List(b)) -> U64")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 20 48) (end 20 49))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "and")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "arguments")
			(reflow " ")
			(reflow "of this call to")
			(reflow " ")
			(annotated code "pair")
			(reflow " ")
			(reflow "have types that conflict with each other."))
		(document
			(source-region (file "call_args_conflict_method_receiver.md") (start 20 48) (end 20 49) (annotation error) (line-text "between_arguments = |handle, x, y| handle.pair(x, y)"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Val(c, Str)")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "call_args_conflict_method_receiver.md") (start 20 51) (end 20 52) (annotation error) (line-text "between_arguments = |handle, x, y| handle.pair(x, y)"))
			(line-break)
			(reflow "But the")
			(reflow " ")
			(reflow "second")
			(reflow " ")
			(reflow "argument")
			(reflow " ")
			(reflow "has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Val(c, U8)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Each fits the type of")
			(reflow " ")
			(annotated code "pair")
			(reflow " ")
			(reflow "by itself, but that type needs them to agree with each other:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Handle(ctx, a), Val(ctx, b), Val(ctx, b) -> U64")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,Comma,NamedUnderscore,Comma,NamedUnderscore,OpBar,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "In")
				(args
					(ty-var (raw "c"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "In"))
						(ty-var (raw "c"))))))
		(s-type-decl
			(header (name "Val")
				(args
					(ty-var (raw "ctx"))
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Val"))
						(ty-var (raw "a"))))))
		(s-type-decl
			(header (name "Handle")
				(args
					(ty-var (raw "ctx"))
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Handle"))
						(ty-var (raw "a")))))
			(associated
				(s-type-anno (name "lift")
					(ty-fn
						(ty-apply
							(ty (name "Handle"))
							(ty-var (raw "c"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Handle"))
							(ty-apply
								(ty (name "In"))
								(ty-var (raw "c")))
							(ty-var (raw "a")))))
				(s-decl
					(p-ident (raw "lift"))
					(e-lambda
						(args
							(p-tag (raw "Handle")
								(p-ident (raw "a"))))
						(e-apply
							(e-tag (raw "Handle"))
							(e-ident (raw "a")))))
				(s-type-anno (name "count")
					(ty-fn
						(ty-apply
							(ty (name "Handle"))
							(ty-var (raw "ctx"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Val"))
							(ty-var (raw "ctx"))
							(ty-apply
								(ty (name "List"))
								(ty-var (raw "b"))))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "count"))
					(e-lambda
						(args
							(p-ident (raw "_handle"))
							(p-tag (raw "Val")
								(p-ident (raw "rows"))))
						(e-method-call (method ".len")
							(receiver
								(e-ident (raw "rows")))
							(args))))
				(s-type-anno (name "pair")
					(ty-fn
						(ty-apply
							(ty (name "Handle"))
							(ty-var (raw "ctx"))
							(ty-var (raw "a")))
						(ty-apply
							(ty (name "Val"))
							(ty-var (raw "ctx"))
							(ty-var (raw "b")))
						(ty-apply
							(ty (name "Val"))
							(ty-var (raw "ctx"))
							(ty-var (raw "b")))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "pair"))
					(e-lambda
						(args
							(p-ident (raw "_handle"))
							(p-ident (raw "_x"))
							(p-ident (raw "_y")))
						(e-int (raw "0"))))))
		(s-type-anno (name "total")
			(ty-fn
				(ty-apply
					(ty (name "Handle"))
					(ty-var (raw "c"))
					(ty (name "U64")))
				(ty-apply
					(ty (name "Val"))
					(ty-apply
						(ty (name "In"))
						(ty-var (raw "c")))
					(ty-apply
						(ty (name "List"))
						(ty (name "Str"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "total"))
			(e-lambda
				(args
					(p-ident (raw "handle"))
					(p-ident (raw "rows")))
				(e-method-call (method ".count")
					(receiver
						(e-method-call (method ".lift")
							(receiver
								(e-method-call (method ".lift")
									(receiver
										(e-ident (raw "handle")))
									(args)))
							(args)))
					(args
						(e-ident (raw "rows"))))))
		(s-type-anno (name "between_arguments")
			(ty-fn
				(ty-apply
					(ty (name "Handle"))
					(ty-var (raw "c"))
					(ty (name "U64")))
				(ty-apply
					(ty (name "Val"))
					(ty-var (raw "c"))
					(ty (name "Str")))
				(ty-apply
					(ty (name "Val"))
					(ty-var (raw "c"))
					(ty (name "U8")))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "between_arguments"))
			(e-lambda
				(args
					(p-ident (raw "handle"))
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-method-call (method ".pair")
					(receiver
						(e-ident (raw "handle")))
					(args
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "call_args_conflict_method_receiver.Handle.lift"))
		(e-lambda
			(args
				(p-applied-tag))
			(e-tag (name "Handle")
				(args
					(e-lookup-local
						(p-assign (ident "a"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "c"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Handle") (local)
					(ty-apply (name "In") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "call_args_conflict_method_receiver.Handle.count"))
		(e-lambda
			(args
				(p-assign (ident "_handle"))
				(p-applied-tag))
			(e-dispatch-call (method "len") (constraint-fn-var 429)
				(receiver
					(e-lookup-local
						(p-assign (ident "rows"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Val") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "ctx")))
					(ty-apply (name "List") (builtin)
						(ty-rigid-var (name "b"))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "call_args_conflict_method_receiver.Handle.pair"))
		(e-lambda
			(args
				(p-assign (ident "_handle"))
				(p-assign (ident "_x"))
				(p-assign (ident "_y")))
			(e-num (value "0")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a")))
				(ty-apply (name "Val") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "ctx")))
					(ty-rigid-var (name "b")))
				(ty-apply (name "Val") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "ctx")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "total"))
		(e-lambda
			(args
				(p-assign (ident "handle"))
				(p-assign (ident "rows")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "c"))
					(ty-lookup (name "U64") (builtin)))
				(ty-apply (name "Val") (local)
					(ty-apply (name "In") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
					(ty-apply (name "List") (builtin)
						(ty-lookup (name "Str") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "between_arguments"))
		(e-lambda
			(args
				(p-assign (ident "handle"))
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Handle") (local)
					(ty-rigid-var (name "c"))
					(ty-lookup (name "U64") (builtin)))
				(ty-apply (name "Val") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "c")))
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Val") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "c")))
					(ty-lookup (name "U8") (builtin)))
				(ty-lookup (name "U64") (builtin)))))
	(s-nominal-decl
		(ty-header (name "In")
			(ty-args
				(ty-rigid-var (name "c"))))
		(ty-tag-union
			(ty-tag-name (name "In")
				(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))
	(s-nominal-decl
		(ty-header (name "Val")
			(ty-args
				(ty-rigid-var (name "ctx"))
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Val")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-nominal-decl
		(ty-header (name "Handle")
			(ty-args
				(ty-rigid-var (name "ctx"))
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Handle")
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Handle(c, a) -> Handle(In(c), a)"))
		(patt (type "Handle(ctx, a), Val(ctx, List(b)) -> U64"))
		(patt (type "Handle(ctx, a), Val(ctx, b), Val(ctx, b) -> U64"))
		(patt (type "Handle(c, U64), Val(In(c), List(Str)) -> U64"))
		(patt (type "Handle(c, U64), Val(c, Str), Val(c, U8) -> U64")))
	(type_decls
		(nominal (type "In(c)")
			(ty-header (name "In")
				(ty-args
					(ty-rigid-var (name "c")))))
		(nominal (type "Val(ctx, a)")
			(ty-header (name "Val")
				(ty-args
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a")))))
		(nominal (type "Handle(ctx, a)")
			(ty-header (name "Handle")
				(ty-args
					(ty-rigid-var (name "ctx"))
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Handle(c, a) -> Handle(In(c), a)"))
		(expr (type "Handle(ctx, a), Val(ctx, List(b)) -> U64"))
		(expr (type "Handle(ctx, a), Val(ctx, b), Val(ctx, b) -> U64"))
		(expr (type "Handle(c, U64), Val(In(c), List(Str)) -> U64"))
		(expr (type "Handle(c, U64), Val(c, Str), Val(c, U8) -> U64"))))
~~~
