# META
~~~ini
description=A function value destructured from a record is called into an annotated result that a caller widens, exactly as a field read is
type=snippet
~~~
# SOURCE
~~~roc
Stepper :: { step : () -> [A, B] }.{
	by_pattern : Stepper -> [A, B]
	by_pattern = |s| match s {
		{ step } => step()
	}

	by_statement : Stepper -> [A, B]
	by_statement = |s| {
		{ step } = s
		step()
	}

	by_param : Stepper -> [A, B]
	by_param = |{ step }| step()

	by_field : Stepper -> [A, B]
	by_field = |s| (s.step)()
}

describe = |s| {
	a = match s.by_pattern() {
		A => "a"
		B => "b"
		C => "c"
	}
	b = match s.by_statement() {
		A => "a"
		B => "b"
		C => "c"
	}
	c = match s.by_param() {
		A => "a"
		B => "b"
		C => "c"
	}
	d = match s.by_field() {
		A => "a"
		B => "b"
		C => "c"
	}
	Str.concat(Str.concat(a, b), Str.concat(c, d))
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpDoubleColon,OpenCurly,LowerIdent,OpColon,OpenRound,CloseRound,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
LowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,CloseCurly,OpBar,LowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Stepper")
				(args))
			(ty-record
				(anno-record-field (name "step")
					(ty-fn
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B")))))))
			(associated
				(s-type-anno (name "by_pattern")
					(ty-fn
						(ty (name "Stepper"))
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B"))))))
				(s-decl
					(p-ident (raw "by_pattern"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-match
							(e-ident (raw "s"))
							(branches
								(branch
									(p-record
										(field (name "step") (rest false)))
									(e-apply
										(e-ident (raw "step"))))))))
				(s-type-anno (name "by_statement")
					(ty-fn
						(ty (name "Stepper"))
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B"))))))
				(s-decl
					(p-ident (raw "by_statement"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-block
							(statements
								(s-decl
									(p-record
										(field (name "step") (rest false)))
									(e-ident (raw "s")))
								(e-apply
									(e-ident (raw "step")))))))
				(s-type-anno (name "by_param")
					(ty-fn
						(ty (name "Stepper"))
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B"))))))
				(s-decl
					(p-ident (raw "by_param"))
					(e-lambda
						(args
							(p-record
								(field (name "step") (rest false))))
						(e-apply
							(e-ident (raw "step")))))
				(s-type-anno (name "by_field")
					(ty-fn
						(ty (name "Stepper"))
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B"))))))
				(s-decl
					(p-ident (raw "by_field"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-apply
							(e-tuple
								(e-field-access
									(receiver
										(e-ident (raw "s")))
									(segment (mode "required") (field "step")))))))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "a"))
							(e-match
								(e-method-call (method ".by_pattern")
									(receiver
										(e-ident (raw "s")))
									(args))
								(branches
									(branch
										(p-tag (raw "A"))
										(e-string
											(e-string-part (raw "a"))))
									(branch
										(p-tag (raw "B"))
										(e-string
											(e-string-part (raw "b"))))
									(branch
										(p-tag (raw "C"))
										(e-string
											(e-string-part (raw "c")))))))
						(s-decl
							(p-ident (raw "b"))
							(e-match
								(e-method-call (method ".by_statement")
									(receiver
										(e-ident (raw "s")))
									(args))
								(branches
									(branch
										(p-tag (raw "A"))
										(e-string
											(e-string-part (raw "a"))))
									(branch
										(p-tag (raw "B"))
										(e-string
											(e-string-part (raw "b"))))
									(branch
										(p-tag (raw "C"))
										(e-string
											(e-string-part (raw "c")))))))
						(s-decl
							(p-ident (raw "c"))
							(e-match
								(e-method-call (method ".by_param")
									(receiver
										(e-ident (raw "s")))
									(args))
								(branches
									(branch
										(p-tag (raw "A"))
										(e-string
											(e-string-part (raw "a"))))
									(branch
										(p-tag (raw "B"))
										(e-string
											(e-string-part (raw "b"))))
									(branch
										(p-tag (raw "C"))
										(e-string
											(e-string-part (raw "c")))))))
						(s-decl
							(p-ident (raw "d"))
							(e-match
								(e-method-call (method ".by_field")
									(receiver
										(e-ident (raw "s")))
									(args))
								(branches
									(branch
										(p-tag (raw "A"))
										(e-string
											(e-string-part (raw "a"))))
									(branch
										(p-tag (raw "B"))
										(e-string
											(e-string-part (raw "b"))))
									(branch
										(p-tag (raw "C"))
										(e-string
											(e-string-part (raw "c")))))))
						(e-apply
							(e-ident (raw "Str.concat"))
							(e-apply
								(e-ident (raw "Str.concat"))
								(e-ident (raw "a"))
								(e-ident (raw "b")))
							(e-apply
								(e-ident (raw "Str.concat"))
								(e-ident (raw "c"))
								(e-ident (raw "d"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "record_destructure_binder_widened_result.Stepper.by_pattern"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "s"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-record-destructure
										(destructs
											(record-destruct (label "step") (ident "step")
												(required
													(p-assign (ident "step"))))))))
							(value
								(e-call (constraint-fn-var 417)
									(e-lookup-local
										(p-assign (ident "step"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "record_destructure_binder_widened_result.Stepper.by_statement"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-record-destructure
						(destructs
							(record-destruct (label "step") (ident "step")
								(required
									(p-assign (ident "step"))))))
					(e-lookup-local
						(p-assign (ident "s"))))
				(e-call (constraint-fn-var 439)
					(e-lookup-local
						(p-assign (ident "step"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "record_destructure_binder_widened_result.Stepper.by_param"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "step") (ident "step")
							(required
								(p-assign (ident "step")))))))
			(e-call (constraint-fn-var 459)
				(e-lookup-local
					(p-assign (ident "step")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "record_destructure_binder_widened_result.Stepper.by_field"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-call (constraint-fn-var 475)
				(e-field-access
					(receiver
						(e-lookup-local
							(p-assign (ident "s"))))
					(segments
						(segment (name "step") (mode "required"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "a"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "by_pattern") (constraint-fn-var 478)
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(args)))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "b")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))))))
				(s-let
					(p-assign (ident "b"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "by_statement") (constraint-fn-var 514)
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(args)))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "b")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))))))
				(s-let
					(p-assign (ident "c"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "by_param") (constraint-fn-var 550)
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(args)))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "b")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))))))
				(s-let
					(p-assign (ident "d"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "by_field") (constraint-fn-var 586)
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(args)))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "a")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "b")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "c")))))))))
				(e-call (constraint-fn-var 632)
					(e-lookup-external
						(builtin))
					(e-call (constraint-fn-var 628)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "a")))
						(e-lookup-local
							(p-assign (ident "b"))))
					(e-call (constraint-fn-var 631)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "c")))
						(e-lookup-local
							(p-assign (ident "d"))))))))
	(s-nominal-decl
		(ty-header (name "Stepper"))
		(ty-record
			(field (field "step")
				(ty-fn (effectful false)
					(ty-tag-union
						(ty-tag-name (name "A"))
						(ty-tag-name (name "B"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Stepper -> [A, B]"))
		(patt (type "Stepper -> [A, B]"))
		(patt (type "Stepper -> [A, B]"))
		(patt (type "Stepper -> [A, B]"))
		(patt (type "e -> Str where [e.by_field : e -> [A, B, C], e.by_param : e -> [A, B, C], e.by_pattern : e -> [A, B, C], e.by_statement : e -> [A, B, C]]")))
	(type_decls
		(nominal (type "Stepper")
			(ty-header (name "Stepper"))))
	(expressions
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "e -> Str where [e.by_field : e -> [A, B, C], e.by_param : e -> [A, B, C], e.by_pattern : e -> [A, B, C], e.by_statement : e -> [A, B, C]]"))))
~~~
