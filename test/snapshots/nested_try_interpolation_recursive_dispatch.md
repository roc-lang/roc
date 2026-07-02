# META
~~~ini
description=Nested Try interpolation reports recursive dispatch
type=snippet
~~~
# SOURCE
~~~roc
Url := [Url(Str)].{
    from_interpolation : Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])
    from_interpolation = |first, rest| Ok(Url.Url(rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment))))
}

main = {
    domain = "example"
    url : Try(Try(Url, [InvalidUrl]), [Outer])
    url = "https://${domain}.com"
    url
}
~~~
# EXPECTED
RECURSIVE DISPATCH - nested_try_interpolation_recursive_dispatch.md:9:11:9:34
# PROBLEMS

┌────────────────────┐
│ RECURSIVE DISPATCH ├─ This `from_interpolation` dispatch would have to ─────┐
└┬───────────────────┘  call itself to satisfy its own type.                  │
 │                                                                            │
 │  url = "https://${domain}.com"                                             │
 │        ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └─────────────────────── nested_try_interpolation_recursive_dispatch.md:9:11 ┘

    The dispatcher type is:

        Try(Url, [InvalidUrl])

    Hint: Use a more specific result type, or add an associated function whose
    `from_interpolation` implementation does not require the same dispatch on
    the same type.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,LowerIdent,Comma,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Url")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Url"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "from_interpolation")
					(ty-fn
						(ty (name "Str"))
						(ty-apply
							(ty (name "Iter"))
							(ty-tuple
								(ty (name "Str"))
								(ty (name "Str"))))
						(ty-apply
							(ty (name "Try"))
							(ty (name "Url"))
							(ty-tag-union
								(tags
									(ty (name "InvalidUrl")))))))
				(s-decl
					(p-ident (raw "from_interpolation"))
					(e-lambda
						(args
							(p-ident (raw "first"))
							(p-ident (raw "rest")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-apply
								(e-tag (raw "Url.Url"))
								(e-method-call (method ".fold")
									(receiver
										(e-ident (raw "rest")))
									(args
										(e-ident (raw "first"))
										(e-lambda
											(args
												(p-ident (raw "acc"))
												(p-tuple
													(p-ident (raw "interpolated"))
													(p-ident (raw "segment"))))
											(e-method-call (method ".concat")
												(receiver
													(e-method-call (method ".concat")
														(receiver
															(e-ident (raw "acc")))
														(args
															(e-ident (raw "interpolated")))))
												(args
													(e-ident (raw "segment")))))))))))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "domain"))
						(e-string
							(e-string-part (raw "example"))))
					(s-type-anno (name "url")
						(ty-apply
							(ty (name "Try"))
							(ty-apply
								(ty (name "Try"))
								(ty (name "Url"))
								(ty-tag-union
									(tags
										(ty (name "InvalidUrl")))))
							(ty-tag-union
								(tags
									(ty (name "Outer"))))))
					(s-decl
						(p-ident (raw "url"))
						(e-string
							(e-string-part (raw "https://"))
							(e-ident (raw "domain"))
							(e-string-part (raw ".com"))))
					(e-ident (raw "url")))))))
~~~
# FORMATTED
~~~roc
Url := [Url(Str)].{
	from_interpolation : Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])
	from_interpolation = |first, rest| Ok(Url.Url(rest.fold(first, |acc, (interpolated, segment)| acc.concat(interpolated).concat(segment))))
}

main = {
	domain = "example"
	url : Try(Try(Url, [InvalidUrl]), [Outer])
	url = "https://${domain}.com"
	url
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nested_try_interpolation_recursive_dispatch.Url.from_interpolation"))
		(e-lambda
			(args
				(p-assign (ident "first"))
				(p-assign (ident "rest")))
			(e-tag (name "Ok")
				(args
					(e-nominal (nominal "Url")
						(e-tag (name "Url")
							(args
								(e-dispatch-call (method "fold") (constraint-fn-var 186)
									(receiver
										(e-lookup-local
											(p-assign (ident "rest"))))
									(args
										(e-lookup-local
											(p-assign (ident "first")))
										(e-lambda
											(args
												(p-assign (ident "acc"))
												(p-tuple
													(patterns
														(p-assign (ident "interpolated"))
														(p-assign (ident "segment")))))
											(e-dispatch-call (method "concat") (constraint-fn-var 184)
												(receiver
													(e-dispatch-call (method "concat") (constraint-fn-var 182)
														(receiver
															(e-lookup-local
																(p-assign (ident "acc"))))
														(args
															(e-lookup-local
																(p-assign (ident "interpolated"))))))
												(args
													(e-lookup-local
														(p-assign (ident "segment"))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Iter") (builtin)
					(ty-tuple
						(ty-lookup (name "Str") (builtin))
						(ty-lookup (name "Str") (builtin))))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Url") (local))
					(ty-tag-union
						(ty-tag-name (name "InvalidUrl")))))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "domain"))
				(e-string
					(e-literal (string "example"))))
			(s-let
				(p-assign (ident "url"))
				(e-block
					(s-let
						(p-assign (ident "#interp_0"))
						(e-lookup-local
							(p-assign (ident "domain"))))
					(e-interpolation (constraint-fn-var 387)
						(first
							(e-literal (string "https://")))
						(parts
							(e-lookup-local
								(p-assign (ident "#interp_0")))
							(e-literal (string ".com"))))))
			(e-lookup-local
				(p-assign (ident "url")))))
	(s-nominal-decl
		(ty-header (name "Url"))
		(ty-tag-union
			(ty-tag-name (name "Url")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])"))
		(patt (type "Try(Try(Url, [InvalidUrl]), [Outer])")))
	(type_decls
		(nominal (type "Url")
			(ty-header (name "Url"))))
	(expressions
		(expr (type "Str, Iter((Str, Str)) -> Try(Url, [InvalidUrl])"))
		(expr (type "Try(Try(Url, [InvalidUrl]), [Outer])"))))
~~~
