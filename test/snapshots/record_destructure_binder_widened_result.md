# META
~~~ini
description=A function value destructured from a record, a record literal or a call result is called into an annotated result that a caller widens, exactly as a field read is
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

	by_literal : Stepper -> [A, B]
	by_literal = |s| {
		{ step } = { step: s.step }
		step()
	}

	by_call : Stepper -> [A, B]
	by_call = |s| {
		{ step } = wrap(s.step)
		step()
	}
}

wrap = |f| { step: f }

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
	e = match s.by_literal() {
		A => "a"
		B => "b"
		C => "c"
	}
	f = match s.by_call() {
		A => "a"
		B => "b"
		C => "c"
	}
	Str.concat(Str.concat(Str.concat(a, b), Str.concat(c, d)), Str.concat(e, f))
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
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
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
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,Comma,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseRound,
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
									(segment (mode "required") (field "step")))))))
				(s-type-anno (name "by_literal")
					(ty-fn
						(ty (name "Stepper"))
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B"))))))
				(s-decl
					(p-ident (raw "by_literal"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-block
							(statements
								(s-decl
									(p-record
										(field (name "step") (rest false)))
									(e-record
										(field (field "step")
											(e-field-access
												(receiver
													(e-ident (raw "s")))
												(segment (mode "required") (field "step"))))))
								(e-apply
									(e-ident (raw "step")))))))
				(s-type-anno (name "by_call")
					(ty-fn
						(ty (name "Stepper"))
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B"))))))
				(s-decl
					(p-ident (raw "by_call"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-block
							(statements
								(s-decl
									(p-record
										(field (name "step") (rest false)))
									(e-apply
										(e-ident (raw "wrap"))
										(e-field-access
											(receiver
												(e-ident (raw "s")))
											(segment (mode "required") (field "step")))))
								(e-apply
									(e-ident (raw "step")))))))))
		(s-decl
			(p-ident (raw "wrap"))
			(e-lambda
				(args
					(p-ident (raw "f")))
				(e-record
					(field (field "step")
						(e-ident (raw "f"))))))
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
						(s-decl
							(p-ident (raw "e"))
							(e-match
								(e-method-call (method ".by_literal")
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
							(p-ident (raw "f"))
							(e-match
								(e-method-call (method ".by_call")
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
								(e-apply
									(e-ident (raw "Str.concat"))
									(e-ident (raw "a"))
									(e-ident (raw "b")))
								(e-apply
									(e-ident (raw "Str.concat"))
									(e-ident (raw "c"))
									(e-ident (raw "d"))))
							(e-apply
								(e-ident (raw "Str.concat"))
								(e-ident (raw "e"))
								(e-ident (raw "f"))))))))))
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
								(e-call (constraint-fn-var 524)
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
				(e-call (constraint-fn-var 546)
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
			(e-call (constraint-fn-var 566)
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
			(e-call (constraint-fn-var 582)
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
		(p-assign (ident "record_destructure_binder_widened_result.Stepper.by_literal"))
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
					(e-record
						(fields
							(field (name "step")
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(segments
										(segment (name "step") (mode "required"))))))))
				(e-call (constraint-fn-var 605)
					(e-lookup-local
						(p-assign (ident "step"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "record_destructure_binder_widened_result.Stepper.by_call"))
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
					(e-call (constraint-fn-var 631)
						(e-lookup-local
							(p-assign (ident "wrap")))
						(e-field-access
							(receiver
								(e-lookup-local
									(p-assign (ident "s"))))
							(segments
								(segment (name "step") (mode "required"))))))
				(e-call (constraint-fn-var 634)
					(e-lookup-local
						(p-assign (ident "step"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-tag-union
					(ty-tag-name (name "A"))
					(ty-tag-name (name "B"))))))
	(d-let
		(p-assign (ident "wrap"))
		(e-lambda
			(args
				(p-assign (ident "f")))
			(e-record
				(fields
					(field (name "step")
						(e-lookup-local
							(p-assign (ident "f"))))))))
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
								(e-dispatch-call (method "by_pattern") (constraint-fn-var 638)
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
								(e-dispatch-call (method "by_statement") (constraint-fn-var 674)
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
								(e-dispatch-call (method "by_param") (constraint-fn-var 710)
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
								(e-dispatch-call (method "by_field") (constraint-fn-var 746)
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
					(p-assign (ident "e"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "by_literal") (constraint-fn-var 782)
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
					(p-assign (ident "f"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "by_call") (constraint-fn-var 818)
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
				(e-call (constraint-fn-var 870)
					(e-lookup-external
						(builtin))
					(e-call (constraint-fn-var 866)
						(e-lookup-external
							(builtin))
						(e-call (constraint-fn-var 862)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "a")))
							(e-lookup-local
								(p-assign (ident "b"))))
						(e-call (constraint-fn-var 865)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "c")))
							(e-lookup-local
								(p-assign (ident "d")))))
					(e-call (constraint-fn-var 869)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "e")))
						(e-lookup-local
							(p-assign (ident "f"))))))))
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
		(patt (type "Stepper -> [A, B]"))
		(patt (type "Stepper -> [A, B]"))
		(patt (type "g -> { step: g }"))
		(patt (type "g -> Str where [g.by_call : g -> [A, B, C], g.by_field : g -> [A, B, C], g.by_literal : g -> [A, B, C], g.by_param : g -> [A, B, C], g.by_pattern : g -> [A, B, C], g.by_statement : g -> [A, B, C]]")))
	(type_decls
		(nominal (type "Stepper")
			(ty-header (name "Stepper"))))
	(expressions
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "Stepper -> [A, B]"))
		(expr (type "g -> { step: g }"))
		(expr (type "g -> Str where [g.by_call : g -> [A, B, C], g.by_field : g -> [A, B, C], g.by_literal : g -> [A, B, C], g.by_param : g -> [A, B, C], g.by_pattern : g -> [A, B, C], g.by_statement : g -> [A, B, C]]"))))
~~~
