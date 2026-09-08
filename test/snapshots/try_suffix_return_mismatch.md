# META
~~~ini
description=A ? whose early return does not match the function body names the Err payload, and points at a trailing ? when the body ends with one
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/11030
parse : Str -> Try(Str, [BadInput])
parse = |s| if s == "" Err(BadInput) else Ok(s)

# The trailing `?` unwraps the Try the body would otherwise return, so the
# earlier `?` has nothing compatible to return into.
ends_with_try = |s| {
	t = parse(s)?
	parse(t)?
}

# The body is not a Try at all.
not_a_try = |s| {
	t = parse(s)?
	Str.concat(t, "!")
}

# The `?` is on a dispatch call whose receiver is only known later in the body.
dispatched = |xs| {
	_x = xs.first()?
	List.len(xs)
}

# A closure argument has its parameter seeded from the method's parameter
# type before its body is checked, so the `?` inside names a concrete error.
closure_arg = |xs| {
	ys : List(List(U64))
	ys = xs
	ys.map_try(|l| {
		_x = l.first()?
		{}
	})
}
~~~
# EXPECTED
TRAILING `?` - try_suffix_return_mismatch.md:9:10:9:11
TYPE MISMATCH - try_suffix_return_mismatch.md:8:6:8:15
TYPE MISMATCH - try_suffix_return_mismatch.md:14:6:14:15
TYPE MISMATCH - try_suffix_return_mismatch.md:20:7:20:18
TYPE MISMATCH - try_suffix_return_mismatch.md:30:8:30:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Trailing `?`")
		(region (start 9 10) (end 9 11))
		(headline
			(reflow "It's usually a mistake to use a postfix ")
			(annotated code "?")
			(reflow " on values being returned implicitly at the end of a function like this:"))
		(document
			(source-region (file "try_suffix_return_mismatch.md") (start 9 10) (end 9 11) (annotation warning) (line-text "\tparse(t)?"))
			(line-break)
			(reflow "This is because ")
			(annotated code "?")
			(reflow " is syntax sugar for doing a ")
			(annotated code "match")
			(reflow " on a ")
			(annotated code "Try")
			(reflow " value like this:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(annotated keyword "match")
			(text " ")
			(annotated symbol-unqualified "value_before_question_mark")
			(text " {")
			(line-break)
			(indent 2)
			(annotated tag "Ok")
			(text "(")
			(annotated symbol-unqualified "ok_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated symbol-unqualified "ok_payload")
			(line-break)
			(indent 2)
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated keyword "return")
			(text " ")
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ")")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "When you use ")
			(annotated code "?")
			(reflow " on the value at the end of a function, it changes \"implicitly return this ")
			(annotated code "Try")
			(reflow " value\" to \"return this ")
			(annotated code "Try")
			(reflow " value if it's an ")
			(annotated code "Err")
			(reflow ", but if it's ")
			(annotated code "Ok")
			(reflow ", unwrap its ")
			(annotated code "Ok")
			(reflow " payload and return that instead\" - which can only possibly type-check when returning ")
			(annotated code "Try(Try(..., ...), ...)")
			(reflow ", which is so unusual that using ")
			(annotated code "?")
			(reflow " here is almost always a mistake in practice.")
			(line-break)
			(line-break)
			(reflow "Usually removing the ")
			(annotated code "?")
			(reflow " here is what makes the most sense, but if you really want this behavior, make it clear by using an explicit ")
			(annotated code "match")
			(reflow " instead of the ")
			(annotated code "?")
			(reflow " syntax sugar.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 8 6) (end 8 15))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_return_mismatch.md") (start 8 6) (end 8 15) (annotation error) (line-text "\tt = parse(s)?"))
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
			(text "[BadInput]")
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
			(reflow "The function body ends with a")
			(reflow " ")
			(annotated code "?")
			(reflow ":")
			(line-break)
			(line-break)
			(source-region (file "try_suffix_return_mismatch.md") (start 9 10) (end 9 11) (annotation error) (line-text "\tparse(t)?"))
			(line-break)
			(reflow "That")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "unwraps the")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "the body would otherwise return. Removing it may fix this.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 14 6) (end 14 15))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_return_mismatch.md") (start 14 6) (end 14 15) (annotation error) (line-text "\tt = parse(s)?"))
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
			(text "[BadInput]")
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
		(region (start 20 7) (end 20 18))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_return_mismatch.md") (start 20 7) (end 20 18) (annotation error) (line-text "\t_x = xs.first()?"))
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
			(text "[ListWasEmpty, ..]")
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
			(text "U64")
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
		(region (start 30 8) (end 30 18))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_return_mismatch.md") (start 30 8) (end 30 18) (annotation error) (line-text "\t\t_x = l.first()?"))
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
			(text "[ListWasEmpty, ..]")
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
			(text "{}")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The error types from all")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "operators and the function body must be compatible, since any of them could be the actual return value."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,StringStart,StringPart,StringEnd,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpQuestion,
OpenCurly,CloseCurly,
CloseCurly,CloseRound,
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
							(ty (name "BadInput")))))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "s"))
						(e-string
							(e-string-part (raw ""))))
					(e-apply
						(e-tag (raw "Err"))
						(e-tag (raw "BadInput")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-ident (raw "s"))))))
		(s-decl
			(p-ident (raw "ends_with_try"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "t"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse"))
									(e-ident (raw "s")))))
						(e-question-suffix
							(e-apply
								(e-ident (raw "parse"))
								(e-ident (raw "t"))))))))
		(s-decl
			(p-ident (raw "not_a_try"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "t"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse"))
									(e-ident (raw "s")))))
						(e-apply
							(e-ident (raw "Str.concat"))
							(e-ident (raw "t"))
							(e-string
								(e-string-part (raw "!"))))))))
		(s-decl
			(p-ident (raw "dispatched"))
			(e-lambda
				(args
					(p-ident (raw "xs")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "_x"))
							(e-question-suffix
								(e-method-call (method ".first")
									(receiver
										(e-ident (raw "xs")))
									(args))))
						(e-apply
							(e-ident (raw "List.len"))
							(e-ident (raw "xs")))))))
		(s-decl
			(p-ident (raw "closure_arg"))
			(e-lambda
				(args
					(p-ident (raw "xs")))
				(e-block
					(statements
						(s-type-anno (name "ys")
							(ty-apply
								(ty (name "List"))
								(ty-apply
									(ty (name "List"))
									(ty (name "U64")))))
						(s-decl
							(p-ident (raw "ys"))
							(e-ident (raw "xs")))
						(e-method-call (method ".map_try")
							(receiver
								(e-ident (raw "ys")))
							(args
								(e-lambda
									(args
										(p-ident (raw "l")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "_x"))
												(e-question-suffix
													(e-method-call (method ".first")
														(receiver
															(e-ident (raw "l")))
														(args))))
											(e-record))))))))))))
~~~
# FORMATTED
~~~roc
# repro for https://github.com/roc-lang/roc/issues/11030
parse : Str -> Try(Str, [BadInput])
parse = |s| if s == "" Err(BadInput) else Ok(s)

# The trailing `?` unwraps the Try the body would otherwise return, so the
# earlier `?` has nothing compatible to return into.
ends_with_try = |s| {
	t = parse(s)?
	parse(t)?
}

# The body is not a Try at all.
not_a_try = |s| {
	t = parse(s)?
	Str.concat(t, "!")
}

# The `?` is on a dispatch call whose receiver is only known later in the body.
dispatched = |xs| {
	_x = xs.first()?
	List.len(xs)
}

# A closure argument has its parameter seeded from the method's parameter
# type before its body is checked, so the `?` inside names a concrete error.
closure_arg = |xs| {
	ys : List(List(U64))
	ys = xs
	ys.map_try(
		|l| {
			_x = l.first()?
			{}
		},
	)
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
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "s"))))
							(rhs
								(e-string
									(e-literal (string "")))))
						(e-tag (name "Err")
							(args
								(e-tag (name "BadInput"))))))
				(if-else
					(e-tag (name "Ok")
						(args
							(e-lookup-local
								(p-assign (ident "s"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadInput")))))))
	(d-let
		(p-assign (ident "ends_with_try"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "not_a_try"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "dispatched"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "closure_arg"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Str, [BadInput])"))
		(patt (type "Str -> Error"))
		(patt (type "Str -> Error"))
		(patt (type "List(item) -> Error"))
		(patt (type "List(List(U64)) -> Error")))
	(expressions
		(expr (type "Str -> Try(Str, [BadInput])"))
		(expr (type "Str -> Error"))
		(expr (type "Str -> Error"))
		(expr (type "List(item) -> Error"))
		(expr (type "List(List(U64)) -> Error"))))
~~~
