# META
~~~ini
description=A construction whose payload forwards a closed parameter widens at its uses exactly like its constructing twin, and a use whose row lacks the forwarded tag is still rejected (design.md "Row Subsumption")
type=snippet
~~~
# SOURCE
~~~roc
fwd_err : [A] -> Try(Str, [A])
fwd_err = |x| Err(x)

con_err : [A] -> Try(Str, [A])
con_err = |x| match x {
    A => Err(A)
}

fwd_wrap : [A] -> [W([A])]
fwd_wrap = |x| W(x)

take_err : Try(Str, [A, B]) -> Str
take_err = |_| "err"

take_other : Try(Str, [B]) -> Str
take_other = |_| "other"

take_wrap : [W([A, B])] -> Str
take_wrap = |_| "wrap"

forwarded = take_err(fwd_err(A))
constructed = take_err(con_err(A))
wrapped = take_wrap(fwd_wrap(A))
rejected = take_other(fwd_err(A))
~~~
# EXPECTED
TYPE MISMATCH - row_subsumption_forwarded_payload.md:24:12:24:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 24 23) (end 24 33))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument being passed to this function has the wrong type."))
		(document
			(source-underlines
				(display (file "row_subsumption_forwarded_payload.md") (start 24 12) (end 24 34) (annotation dim) (line-text "rejected = take_other(fwd_err(A))"))
				(underline (start 24 23) (end 24 33) (annotation error)))
			(line-break)
			(reflow "This argument has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(Str, [A])")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But")
			(reflow " ")
			(annotated code "take_other")
			(reflow " ")
			(reflow "needs the")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "argument to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(Str, [B])")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,UpperIdent,CloseSquare,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,OpenSquare,UpperIdent,CloseSquare,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,CloseSquare,OpArrow,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,CloseSquare,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,CloseRound,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "fwd_err")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "A"))))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "A")))))))
		(s-decl
			(p-ident (raw "fwd_err"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-apply
					(e-tag (raw "Err"))
					(e-ident (raw "x")))))
		(s-type-anno (name "con_err")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "A"))))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "A")))))))
		(s-decl
			(p-ident (raw "con_err"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-match
					(e-ident (raw "x"))
					(branches
						(branch
							(p-tag (raw "A"))
							(e-apply
								(e-tag (raw "Err"))
								(e-tag (raw "A"))))))))
		(s-type-anno (name "fwd_wrap")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "A"))))
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "W"))
							(ty-tag-union
								(tags
									(ty (name "A")))))))))
		(s-decl
			(p-ident (raw "fwd_wrap"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-apply
					(e-tag (raw "W"))
					(e-ident (raw "x")))))
		(s-type-anno (name "take_err")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "A"))
							(ty (name "B")))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "take_err"))
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-string-part (raw "err")))))
		(s-type-anno (name "take_other")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "B")))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "take_other"))
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-string-part (raw "other")))))
		(s-type-anno (name "take_wrap")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "W"))
							(ty-tag-union
								(tags
									(ty (name "A"))
									(ty (name "B")))))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "take_wrap"))
			(e-lambda
				(args
					(p-underscore))
				(e-string
					(e-string-part (raw "wrap")))))
		(s-decl
			(p-ident (raw "forwarded"))
			(e-apply
				(e-ident (raw "take_err"))
				(e-apply
					(e-ident (raw "fwd_err"))
					(e-tag (raw "A")))))
		(s-decl
			(p-ident (raw "constructed"))
			(e-apply
				(e-ident (raw "take_err"))
				(e-apply
					(e-ident (raw "con_err"))
					(e-tag (raw "A")))))
		(s-decl
			(p-ident (raw "wrapped"))
			(e-apply
				(e-ident (raw "take_wrap"))
				(e-apply
					(e-ident (raw "fwd_wrap"))
					(e-tag (raw "A")))))
		(s-decl
			(p-ident (raw "rejected"))
			(e-apply
				(e-ident (raw "take_other"))
				(e-apply
					(e-ident (raw "fwd_err"))
					(e-tag (raw "A")))))))
~~~
# FORMATTED
~~~roc
fwd_err : [A] -> Try(Str, [A])
fwd_err = |x| Err(x)

con_err : [A] -> Try(Str, [A])
con_err = |x| match x {
	A => Err(A)
}

fwd_wrap : [A] -> [W([A])]
fwd_wrap = |x| W(x)

take_err : Try(Str, [A, B]) -> Str
take_err = |_| "err"

take_other : Try(Str, [B]) -> Str
take_other = |_| "other"

take_wrap : [W([A, B])] -> Str
take_wrap = |_| "wrap"

forwarded = take_err(fwd_err(A))

constructed = take_err(con_err(A))

wrapped = take_wrap(fwd_wrap(A))

rejected = take_other(fwd_err(A))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "fwd_err"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-tag (name "Err")
				(args
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A")))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "A")))))))
	(d-let
		(p-assign (ident "con_err"))
		(e-lambda
			(args
				(p-assign (ident "x")))
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
								(e-tag (name "Err")
									(args
										(e-tag (name "A"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A")))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "A")))))))
	(d-let
		(p-assign (ident "fwd_wrap"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-tag (name "W")
				(args
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "A")))
				(ty-tag-union
					(ty-tag-name (name "W")
						(ty-tag-union
							(ty-tag-name (name "A"))))))))
	(d-let
		(p-assign (ident "take_err"))
		(e-lambda
			(args
				(p-underscore))
			(e-string
				(e-literal (string "err"))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "A"))
						(ty-tag-name (name "B"))))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "take_other"))
		(e-lambda
			(args
				(p-underscore))
			(e-string
				(e-literal (string "other"))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "B"))))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "take_wrap"))
		(e-lambda
			(args
				(p-underscore))
			(e-string
				(e-literal (string "wrap"))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "W")
						(ty-tag-union
							(ty-tag-name (name "A"))
							(ty-tag-name (name "B")))))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "forwarded"))
		(e-call (constraint-fn-var 513)
			(e-lookup-local
				(p-assign (ident "take_err")))
			(e-call (constraint-fn-var 511)
				(e-lookup-local
					(p-assign (ident "fwd_err")))
				(e-tag (name "A")))))
	(d-let
		(p-assign (ident "constructed"))
		(e-call (constraint-fn-var 527)
			(e-lookup-local
				(p-assign (ident "take_err")))
			(e-call (constraint-fn-var 525)
				(e-lookup-local
					(p-assign (ident "con_err")))
				(e-tag (name "A")))))
	(d-let
		(p-assign (ident "wrapped"))
		(e-call (constraint-fn-var 542)
			(e-lookup-local
				(p-assign (ident "take_wrap")))
			(e-call (constraint-fn-var 540)
				(e-lookup-local
					(p-assign (ident "fwd_wrap")))
				(e-tag (name "A")))))
	(d-let
		(p-assign (ident "rejected"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "[A] -> Try(Str, [A])"))
		(patt (type "[A] -> Try(Str, [A])"))
		(patt (type "[A] -> [W([A])]"))
		(patt (type "Try(Str, [A, B]) -> Str"))
		(patt (type "Try(Str, [B]) -> Str"))
		(patt (type "[W([A, B])] -> Str"))
		(patt (type "Str"))
		(patt (type "Str"))
		(patt (type "Str"))
		(patt (type "Error")))
	(expressions
		(expr (type "[A] -> Try(Str, [A])"))
		(expr (type "[A] -> Try(Str, [A])"))
		(expr (type "[A] -> [W([A])]"))
		(expr (type "Try(Str, [A, B]) -> Str"))
		(expr (type "Try(Str, [B]) -> Str"))
		(expr (type "[W([A, B])] -> Str"))
		(expr (type "Str"))
		(expr (type "Str"))
		(expr (type "Str"))
		(expr (type "Error"))))
~~~
