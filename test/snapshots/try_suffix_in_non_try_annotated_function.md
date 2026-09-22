# META
~~~ini
description=A try suffix inside a function annotated with a non-Try result reports against that result, whether or not the body is a Try
type=snippet
~~~
# SOURCE
~~~roc
parse : Str -> Try(Str, [Bad])
parse = |s| if Str.is_empty(s) { Err(Bad) } else { Ok(s) }

plain : Str -> Str
plain = |s| {
	x = parse(s)?
	x
}

wrapped : Str -> Str
wrapped = |s| {
	_ = parse(s)?
	Ok(s)
}
~~~
# EXPECTED
TYPE MISMATCH - try_suffix_in_non_try_annotated_function.md:6:6:6:15
TYPE MISMATCH - try_suffix_in_non_try_annotated_function.md:13:2:13:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 6) (end 6 15))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_in_non_try_annotated_function.md") (start 6 6) (end 6 15) (annotation error) (line-text "\tx = parse(s)?"))
			(line-break)
			(reflow "If this")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "is an")
			(reflow " ")
			(annotated code "Err")
			(reflow ",")
			(reflow " ")
			(reflow "then the")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "after it immediately returns an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "whose payload has this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Bad]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Returning an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "with that type only works if the function itself returns a")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "with a compatible error type, but this function's return type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The error types from all")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "operators and the function body must be compatible, since any of them could be the actual return value.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 13 2) (end 13 7))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "try_suffix_in_non_try_annotated_function.md") (start 13 2) (end 13 7) (annotation error) (line-text "\tOk(s)"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Ok(Str)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the annotation says it should be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "parse")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-if-then-else
					(e-apply
						(e-ident (raw "Str.is_empty"))
						(e-ident (raw "s")))
					(e-block
						(statements
							(e-apply
								(e-tag (raw "Err"))
								(e-tag (raw "Bad")))))
					(e-block
						(statements
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "s"))))))))
		(s-type-anno (name "plain")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "plain"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "x"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse"))
									(e-ident (raw "s")))))
						(e-ident (raw "x"))))))
		(s-type-anno (name "wrapped")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "wrapped"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse"))
									(e-ident (raw "s")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "s")))))))))
~~~
# FORMATTED
~~~roc
parse : Str -> Try(Str, [Bad])
parse = |s| if Str.is_empty(s) {
	Err(Bad)
} else {
	Ok(s)
}

plain : Str -> Str
plain = |s| {
	x = parse(s)?
	x
}

wrapped : Str -> Str
wrapped = |s| {
	_ = parse(s)?
	Ok(s)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-if
				(if-branches
					(if-branch
						(e-call (constraint-fn-var 331)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "s"))))
						(e-block
							(e-tag (name "Err")
								(args
									(e-tag (name "Bad")))))))
				(if-else
					(e-block
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "s")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Bad")))))))
	(d-let
		(p-assign (ident "plain"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "wrapped"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Str, [Bad])"))
		(patt (type "Str -> Str"))
		(patt (type "Str -> Str")))
	(expressions
		(expr (type "Str -> Try(Str, [Bad])"))
		(expr (type "Str -> Str"))
		(expr (type "Str -> Str"))))
~~~
