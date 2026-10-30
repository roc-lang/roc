# META
~~~ini
description=A nested annotated function whose body's type is still a variable of the enclosing function relates to its annotation at the enclosing boundary, so calls inside that function widen the annotation as written
type=snippet
~~~
# SOURCE
~~~roc
Stepper :: { step : () -> [A, B] }.{
	by_destructure : Stepper -> Str
	by_destructure = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| {
			{ step } = r
			step()
		}
		match inner({}) {
			A => "a"
			B => "b"
			C => "c"
		}
	}

	by_field : Stepper -> Str
	by_field = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| (r.step)()
		match inner({}) {
			A => "a"
			B => "b"
			C => "c"
		}
	}
}

by_param = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match inner({}) {
		A => "a"
		B => "b"
		C => "c"
	}
}

narrowed_later = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match x {
		A => inner({})
		B => B
	}
}

by_branch = |x, flag| {
	inner : {} -> [A, B]
	inner = |_| if flag x else A
	match inner({}) {
		A => "a"
		B => "b"
		C => "c"
	}
}

optional_later : { step : () -> [A, B] } -> Try(() -> [A, B], [MissingField])
optional_later = |s| {
	r = { step: s.step }
	inner : {} -> Try(() -> [A, B], [MissingField])
	inner = |_| {
		{ step } = r
		step
	}
	_ = optional(r)
	inner({})
}

optional : { step ?: () -> [A, B] } -> U64
optional = |_| 1

widened_later = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match x {
		A => inner({})
		B => B
		C => C
	}
}

not_polymorphic = |x| {
	inner : {} -> [A(a)]
	inner = |_| A(x)
	inner({})
}
~~~
# EXPECTED
TYPE MISMATCH - deferred_result_relation.md:76:14:76:15
TYPE MISMATCH - deferred_result_relation.md:86:14:86:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 76 14) (end 76 15))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "deferred_result_relation.md") (start 76 14) (end 76 15) (annotation error) (line-text "\tinner = |_| x"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, B, C]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A, B]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Maybe")
			(reflow " ")
			(annotated code "C")
			(reflow " ")
			(reflow "should be")
			(reflow " ")
			(annotated code "A")
			(reflow "?")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 86 14) (end 86 18))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "deferred_result_relation.md") (start 86 14) (end 86 18) (annotation error) (line-text "\tinner = |_| A(x)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A(_b)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[A(a)]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpDoubleColon,OpenCurly,LowerIdent,OpColon,OpenRound,CloseRound,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
LowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
KwMatch,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,CloseRound,
KwMatch,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
KwMatch,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,KwIf,LowerIdent,LowerIdent,KwElse,UpperIdent,
KwMatch,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenRound,CloseRound,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,CloseCurly,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,CloseRound,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,CloseRound,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,OpAssign,LowerIdent,
LowerIdent,
CloseCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpQuestion,OpColon,OpenRound,CloseRound,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,LowerIdent,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
UpperIdent,OpFatArrow,UpperIdent,
UpperIdent,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
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
				(s-type-anno (name "by_destructure")
					(ty-fn
						(ty (name "Stepper"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "by_destructure"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "r"))
									(e-record
										(field (field "step")
											(e-field-access
												(receiver
													(e-ident (raw "s")))
												(segment (mode "required") (field "step"))))))
								(s-type-anno (name "inner")
									(ty-fn
										(ty-record)
										(ty-tag-union
											(tags
												(ty (name "A"))
												(ty (name "B"))))))
								(s-decl
									(p-ident (raw "inner"))
									(e-lambda
										(args
											(p-underscore))
										(e-block
											(statements
												(s-decl
													(p-record
														(field (name "step") (rest false)))
													(e-ident (raw "r")))
												(e-apply
													(e-ident (raw "step")))))))
								(e-match
									(e-apply
										(e-ident (raw "inner"))
										(e-record))
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
												(e-string-part (raw "c"))))))))))
				(s-type-anno (name "by_field")
					(ty-fn
						(ty (name "Stepper"))
						(ty (name "Str"))))
				(s-decl
					(p-ident (raw "by_field"))
					(e-lambda
						(args
							(p-ident (raw "s")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "r"))
									(e-record
										(field (field "step")
											(e-field-access
												(receiver
													(e-ident (raw "s")))
												(segment (mode "required") (field "step"))))))
								(s-type-anno (name "inner")
									(ty-fn
										(ty-record)
										(ty-tag-union
											(tags
												(ty (name "A"))
												(ty (name "B"))))))
								(s-decl
									(p-ident (raw "inner"))
									(e-lambda
										(args
											(p-underscore))
										(e-apply
											(e-tuple
												(e-field-access
													(receiver
														(e-ident (raw "r")))
													(segment (mode "required") (field "step")))))))
								(e-match
									(e-apply
										(e-ident (raw "inner"))
										(e-record))
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
												(e-string-part (raw "c"))))))))))))
		(s-decl
			(p-ident (raw "by_param"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-record)
								(ty-tag-union
									(tags
										(ty (name "A"))
										(ty (name "B"))))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-underscore))
								(e-ident (raw "x"))))
						(e-match
							(e-apply
								(e-ident (raw "inner"))
								(e-record))
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
										(e-string-part (raw "c"))))))))))
		(s-decl
			(p-ident (raw "narrowed_later"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-record)
								(ty-tag-union
									(tags
										(ty (name "A"))
										(ty (name "B"))))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-underscore))
								(e-ident (raw "x"))))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "A"))
									(e-apply
										(e-ident (raw "inner"))
										(e-record)))
								(branch
									(p-tag (raw "B"))
									(e-tag (raw "B")))))))))
		(s-decl
			(p-ident (raw "by_branch"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "flag")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-record)
								(ty-tag-union
									(tags
										(ty (name "A"))
										(ty (name "B"))))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-underscore))
								(e-if-then-else
									(e-ident (raw "flag"))
									(e-ident (raw "x"))
									(e-tag (raw "A")))))
						(e-match
							(e-apply
								(e-ident (raw "inner"))
								(e-record))
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
										(e-string-part (raw "c"))))))))))
		(s-type-anno (name "optional_later")
			(ty-fn
				(ty-record
					(anno-record-field (name "step")
						(ty-fn
							(ty-tag-union
								(tags
									(ty (name "A"))
									(ty (name "B")))))))
				(ty-apply
					(ty (name "Try"))
					(ty-fn
						(ty-tag-union
							(tags
								(ty (name "A"))
								(ty (name "B")))))
					(ty-tag-union
						(tags
							(ty (name "MissingField")))))))
		(s-decl
			(p-ident (raw "optional_later"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "r"))
							(e-record
								(field (field "step")
									(e-field-access
										(receiver
											(e-ident (raw "s")))
										(segment (mode "required") (field "step"))))))
						(s-type-anno (name "inner")
							(ty-fn
								(ty-record)
								(ty-apply
									(ty (name "Try"))
									(ty-fn
										(ty-tag-union
											(tags
												(ty (name "A"))
												(ty (name "B")))))
									(ty-tag-union
										(tags
											(ty (name "MissingField")))))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-underscore))
								(e-block
									(statements
										(s-decl
											(p-record
												(field (name "step") (rest false)))
											(e-ident (raw "r")))
										(e-ident (raw "step"))))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "optional"))
								(e-ident (raw "r"))))
						(e-apply
							(e-ident (raw "inner"))
							(e-record))))))
		(s-type-anno (name "optional")
			(ty-fn
				(ty-record
					(anno-record-field (name "step") (optional true)
						(ty-fn
							(ty-tag-union
								(tags
									(ty (name "A"))
									(ty (name "B")))))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "optional"))
			(e-lambda
				(args
					(p-underscore))
				(e-int (raw "1"))))
		(s-decl
			(p-ident (raw "widened_later"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-record)
								(ty-tag-union
									(tags
										(ty (name "A"))
										(ty (name "B"))))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-underscore))
								(e-ident (raw "x"))))
						(e-match
							(e-ident (raw "x"))
							(branches
								(branch
									(p-tag (raw "A"))
									(e-apply
										(e-ident (raw "inner"))
										(e-record)))
								(branch
									(p-tag (raw "B"))
									(e-tag (raw "B")))
								(branch
									(p-tag (raw "C"))
									(e-tag (raw "C")))))))))
		(s-decl
			(p-ident (raw "not_polymorphic"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-record)
								(ty-tag-union
									(tags
										(ty-apply
											(ty (name "A"))
											(ty-var (raw "a")))))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-underscore))
								(e-apply
									(e-tag (raw "A"))
									(e-ident (raw "x")))))
						(e-apply
							(e-ident (raw "inner"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
Stepper :: { step : () -> [A, B] }.{
	by_destructure : Stepper -> Str
	by_destructure = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| {
			{ step } = r
			step()
		}
		match inner({}) {
			A => "a"
			B => "b"
			C => "c"
		}
	}

	by_field : Stepper -> Str
	by_field = |s| {
		r = { step: s.step }
		inner : {} -> [A, B]
		inner = |_| (r.step)()
		match inner({}) {
			A => "a"
			B => "b"
			C => "c"
		}
	}
}

by_param = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match inner({}) {
		A => "a"
		B => "b"
		C => "c"
	}
}

narrowed_later = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match x {
		A => inner({})
		B => B
	}
}

by_branch = |x, flag| {
	inner : {} -> [A, B]
	inner = |_| if (flag) x else A
	match inner({}) {
		A => "a"
		B => "b"
		C => "c"
	}
}

optional_later : { step : () -> [A, B] } -> Try(() -> [A, B], [MissingField])
optional_later = |s| {
	r = { step: s.step }
	inner : {} -> Try(() -> [A, B], [MissingField])
	inner = |_| {
		{ step } = r
		step
	}
	_ = optional(r)
	inner({})
}

optional : { step ?: () -> [A, B] } -> U64
optional = |_| 1

widened_later = |x| {
	inner : {} -> [A, B]
	inner = |_| x
	match x {
		A => inner({})
		B => B
		C => C
	}
}

not_polymorphic = |x| {
	inner : {} -> [A(a)]
	inner = |_| A(x)
	inner({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "deferred_result_relation.Stepper.by_destructure"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "r"))
					(e-record
						(fields
							(field (name "step")
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(segments
										(segment (name "step") (mode "required"))))))))
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "r")))
						(e-lambda
							(args
								(p-underscore))
							(e-block
								(s-let
									(p-record-destructure
										(destructs
											(record-destruct (label "step") (ident "step")
												(required
													(p-assign (ident "step"))))))
									(e-lookup-local
										(p-assign (ident "r"))))
								(e-call (constraint-fn-var 636)
									(e-lookup-local
										(p-assign (ident "step"))))))))
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 642)
								(e-lookup-local
									(p-assign (ident "inner")))
								(e-empty_record)))
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
										(e-literal (string "c"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "deferred_result_relation.Stepper.by_field"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "r"))
					(e-record
						(fields
							(field (name "step")
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(segments
										(segment (name "step") (mode "required"))))))))
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "r")))
						(e-lambda
							(args
								(p-underscore))
							(e-call (constraint-fn-var 705)
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "r"))))
									(segments
										(segment (name "step") (mode "required"))))))))
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 712)
								(e-lookup-local
									(p-assign (ident "inner")))
								(e-empty_record)))
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
										(e-literal (string "c"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stepper") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "by_param"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-underscore))
							(e-lookup-local
								(p-assign (ident "x"))))))
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 765)
								(e-lookup-local
									(p-assign (ident "inner")))
								(e-empty_record)))
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
										(e-literal (string "c")))))))))))
	(d-let
		(p-assign (ident "narrowed_later"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-underscore))
							(e-lookup-local
								(p-assign (ident "x"))))))
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
									(e-call (constraint-fn-var 821)
										(e-lookup-local
											(p-assign (ident "inner")))
										(e-empty_record))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "B"))))))))))
	(d-let
		(p-assign (ident "by_branch"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "flag")))
			(e-block
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "flag"))
							(capture (ident "x")))
						(e-lambda
							(args
								(p-underscore))
							(e-if
								(if-branches
									(if-branch
										(e-lookup-local
											(p-assign (ident "flag")))
										(e-lookup-local
											(p-assign (ident "x")))))
								(if-else
									(e-tag (name "A")))))))
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 859)
								(e-lookup-local
									(p-assign (ident "inner")))
								(e-empty_record)))
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
										(e-literal (string "c")))))))))))
	(d-let
		(p-assign (ident "optional_later"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "r"))
					(e-record
						(fields
							(field (name "step")
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "s"))))
									(segments
										(segment (name "step") (mode "required"))))))))
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "r")))
						(e-lambda
							(args
								(p-underscore))
							(e-block
								(s-let
									(p-record-destructure
										(destructs
											(record-destruct (label "step") (ident "step")
												(required
													(p-assign (ident "step"))))))
									(e-lookup-local
										(p-assign (ident "r"))))
								(e-lookup-local
									(p-assign (ident "step")))))))
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 967)
						(e-lookup-local
							(p-assign (ident "optional")))
						(e-lookup-local
							(p-assign (ident "r")))))
				(e-call (constraint-fn-var 975)
					(e-lookup-local
						(p-assign (ident "inner")))
					(e-empty_record))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "step")
						(ty-fn (effectful false)
							(ty-tag-union
								(ty-tag-name (name "A"))
								(ty-tag-name (name "B"))))))
				(ty-apply (name "Try") (builtin)
					(ty-fn (effectful false)
						(ty-tag-union
							(ty-tag-name (name "A"))
							(ty-tag-name (name "B"))))
					(ty-tag-union
						(ty-tag-name (name "MissingField")))))))
	(d-let
		(p-assign (ident "optional"))
		(e-lambda
			(args
				(p-underscore))
			(e-num (value "1")))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "step") (optional true)
						(ty-fn (effectful false)
							(ty-tag-union
								(ty-tag-name (name "A"))
								(ty-tag-name (name "B"))))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "widened_later"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-underscore))
							(e-runtime-error (tag "erroneous_value_use")))))
				(e-match
					(match
						(cond
							(e-runtime-error (tag "erroneous_value_use")))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-call (constraint-fn-var 1008)
										(e-lookup-local
											(p-assign (ident "inner")))
										(e-empty_record))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "B"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "C"))))))))))
	(d-let
		(p-assign (ident "not_polymorphic"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-let
					(p-assign (ident "inner"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-underscore))
							(e-runtime-error (tag "erroneous_value_expr")))))
				(e-call (constraint-fn-var 1042)
					(e-lookup-local
						(p-assign (ident "inner")))
					(e-empty_record)))))
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
		(patt (type "Stepper -> Str"))
		(patt (type "Stepper -> Str"))
		(patt (type "[A, B] -> b where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(patt (type "[A, B] -> [A, B]"))
		(patt (type "[A, B], Bool -> b where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(patt (type "{ step: () -> [A, B] } -> Try(() -> [A, B], [MissingField])"))
		(patt (type "{ step ?: () -> [A, B] } -> U64"))
		(patt (type "Error -> [A, B, C]"))
		(patt (type "Error -> [A(a)]")))
	(type_decls
		(nominal (type "Stepper")
			(ty-header (name "Stepper"))))
	(expressions
		(expr (type "Stepper -> Str"))
		(expr (type "Stepper -> Str"))
		(expr (type "[A, B] -> b where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(expr (type "[A, B] -> [A, B]"))
		(expr (type "[A, B], Bool -> b where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(expr (type "{ step: () -> [A, B] } -> Try(() -> [A, B], [MissingField])"))
		(expr (type "{ step ?: () -> [A, B] } -> U64"))
		(expr (type "Error -> [A, B, C]"))
		(expr (type "Error -> [A(a)]"))))
~~~
