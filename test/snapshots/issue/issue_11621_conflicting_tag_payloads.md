# META
~~~ini
description=A callback raising a helper's own tag with a different payload reports both occurrences of the tag
type=snippet
~~~
# SOURCE
~~~roc
step : U64 -> Try(U64, [StepFailed])
step = |n| if n > 3 { Err(StepFailed) } else { Ok(n) }

describe : U64 -> Try(U64, [StepFailed(Str)])
describe = |n| if n > 3 { Err(StepFailed("too big")) } else { Ok(n) }

apply = |f, n| {
	_ = step(n)?
	f(n + 1)
}

use = |n| apply(describe, n)
~~~
# EXPECTED
CONFLICTING TAG - issue_11621_conflicting_tag_payloads.md:8:6:8:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Conflicting Tag")
		(region (start 8 6) (end 8 14))
		(headline
			(reflow "The")
			(reflow " ")
			(annotated code "StepFailed")
			(reflow " ")
			(reflow "tag")
			(reflow " ")
			(reflow "comes from two places with different")
			(reflow " ")
			(reflow "payloads")
			(reflow "."))
		(document
			(source-region (file "issue_11621_conflicting_tag_payloads.md") (start 8 6) (end 8 14) (annotation error) (line-text "\t_ = step(n)?"))
			(line-break)
			(reflow "Here it is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[StepFailed]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "It also comes from here:")
			(line-break)
			(source-region (file "issue_11621_conflicting_tag_payloads.md") (start 4 19) (end 4 46) (annotation error) (line-text "describe : U64 -> Try(U64, [StepFailed(Str)])"))
			(line-break)
			(line-break)
			(reflow "where it is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[StepFailed(Str)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "A tag union has each tag once, so every occurrence of a tag must have the same payload."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpGreaterThan,Int,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpGreaterThan,Int,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,NoSpaceOpenRound,LowerIdent,OpPlus,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "step")
			(ty-fn
				(ty (name "U64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U64"))
					(ty-tag-union
						(tags
							(ty (name "StepFailed")))))))
		(s-decl
			(p-ident (raw "step"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op ">")
						(e-ident (raw "n"))
						(e-int (raw "3")))
					(e-block
						(statements
							(e-apply
								(e-tag (raw "Err"))
								(e-tag (raw "StepFailed")))))
					(e-block
						(statements
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "n"))))))))
		(s-type-anno (name "describe")
			(ty-fn
				(ty (name "U64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U64"))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "StepFailed"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op ">")
						(e-ident (raw "n"))
						(e-int (raw "3")))
					(e-block
						(statements
							(e-apply
								(e-tag (raw "Err"))
								(e-apply
									(e-tag (raw "StepFailed"))
									(e-string
										(e-string-part (raw "too big")))))))
					(e-block
						(statements
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "n"))))))))
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "f"))
					(p-ident (raw "n")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "step"))
									(e-ident (raw "n")))))
						(e-apply
							(e-ident (raw "f"))
							(e-binop (op "+")
								(e-ident (raw "n"))
								(e-int (raw "1"))))))))
		(s-decl
			(p-ident (raw "use"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "apply"))
					(e-ident (raw "describe"))
					(e-ident (raw "n")))))))
~~~
# FORMATTED
~~~roc
step : U64 -> Try(U64, [StepFailed])
step = |n| if n > 3 {
	Err(StepFailed)
} else {
	Ok(n)
}

describe : U64 -> Try(U64, [StepFailed(Str)])
describe = |n| if n > 3 {
	Err(StepFailed("too big"))
} else {
	Ok(n)
}

apply = |f, n| {
	_ = step(n)?
	f(n + 1)
}

use = |n| apply(describe, n)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "step"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-if
				(if-branches
					(if-branch
						(e-dispatch-call (method "is_gt") (constraint-fn-var 336)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "3"))))
						(e-block
							(e-tag (name "Err")
								(args
									(e-tag (name "StepFailed")))))))
				(if-else
					(e-block
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "n")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "StepFailed")))))))
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-if
				(if-branches
					(if-branch
						(e-dispatch-call (method "is_gt") (constraint-fn-var 380)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "3"))))
						(e-block
							(e-tag (name "Err")
								(args
									(e-tag (name "StepFailed")
										(args
											(e-string
												(e-literal (string "too big"))))))))))
				(if-else
					(e-block
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "n")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "StepFailed")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "f"))
				(p-assign (ident "n")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 409)
									(e-lookup-local
										(p-assign (ident "step")))
									(e-lookup-local
										(p-assign (ident "n")))))
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
										(e-return
											(e-nominal-external
												(builtin)
												(e-tag (name "Err")
													(args
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))
				(e-call (constraint-fn-var 461)
					(e-lookup-local
						(p-assign (ident "f")))
					(e-dispatch-call (method "plus") (constraint-fn-var 455)
						(receiver
							(e-lookup-local
								(p-assign (ident "n"))))
						(args
							(e-num (value "1"))))))))
	(d-let
		(p-assign (ident "use"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-call (constraint-fn-var 477)
				(e-lookup-local
					(p-assign (ident "apply")))
				(e-lookup-local
					(p-assign (ident "describe")))
				(e-lookup-local
					(p-assign (ident "n")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> Try(U64, [StepFailed])"))
		(patt (type "U64 -> Try(U64, [StepFailed(Str)])"))
		(patt (type "(U64 -> Try(ok, a)), U64 -> Try(ok, [StepFailed, ..a])"))
		(patt (type "U64 -> Try(U64, Error)")))
	(expressions
		(expr (type "U64 -> Try(U64, [StepFailed])"))
		(expr (type "U64 -> Try(U64, [StepFailed(Str)])"))
		(expr (type "(U64 -> Try(ok, a)), U64 -> Try(ok, [StepFailed, ..a])"))
		(expr (type "U64 -> Try(U64, Error)"))))
~~~
