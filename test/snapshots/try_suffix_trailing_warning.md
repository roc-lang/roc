# META
~~~ini
description=A ? applied to the value a function returns warns, even where it type-checks by unwrapping a nested Try
type=snippet
~~~
# SOURCE
~~~roc
# A `?` applied to a function's return value only type-checks when the Ok
# payload is itself a Try, and even then it is confusing to read, so it warns.
nested : Str -> Try(Try(Str, [Bad]), [Bad])
nested = |s| if s == "" Err(Bad) else Ok(Ok(s))

tail : Str -> Try(Str, [Bad])
tail = |s| {
	nested(s)?
}

returned : Str -> Try(Str, [Bad])
returned = |s| {
	if s == "x" {
		return nested(s)?
	}
	if s == "y" nested(s)? else Ok(s)
}
~~~
# EXPECTED
TRAILING `?` - try_suffix_trailing_warning.md:8:11:8:12
TRAILING `?` - try_suffix_trailing_warning.md:14:19:14:20
TRAILING `?` - try_suffix_trailing_warning.md:16:23:16:24
# PROBLEMS
── ● trailing `?` ────────────────────────── try_suffix_trailing_warning.md:8:11

It's usually a mistake to use a postfix ? on values being returned implicitly
at the end of a function like this:

nested(s)?
         ^

This is because ? is syntax sugar for doing a match on a Try value like this:

    match value_before_question_mark {
        Ok(ok_payload) => ok_payload
        Err(err_payload) => return Err(err_payload)
    }

When you use ? on the value at the end of a function, it changes "implicitly
return this Try value" to "return this Try value if it's an Err, but if it's
Ok, unwrap its Ok payload and return that instead" - which can only possibly
type-check when returning Try(Try(..., ...), ...), which is so unusual that
using ? here is almost always a mistake in practice.

Usually removing the ? here is what makes the most sense, but if you really
want this behavior, make it clear by using an explicit match instead of the ?
syntax sugar.

── ● trailing `?` ───────────────────────── try_suffix_trailing_warning.md:14:19

It's usually a mistake to use a postfix ? on values being returned implicitly
at the end of a function like this:

return nested(s)?
                ^

This is because ? is syntax sugar for doing a match on a Try value like this:

    match value_before_question_mark {
        Ok(ok_payload) => ok_payload
        Err(err_payload) => return Err(err_payload)
    }

When you use ? on the value at the end of a function, it changes "implicitly
return this Try value" to "return this Try value if it's an Err, but if it's
Ok, unwrap its Ok payload and return that instead" - which can only possibly
type-check when returning Try(Try(..., ...), ...), which is so unusual that
using ? here is almost always a mistake in practice.

Usually removing the ? here is what makes the most sense, but if you really
want this behavior, make it clear by using an explicit match instead of the ?
syntax sugar.

── ● trailing `?` ───────────────────────── try_suffix_trailing_warning.md:16:23

It's usually a mistake to use a postfix ? on values being returned implicitly
at the end of a function like this:

if s == "y" nested(s)? else Ok(s)
                     ^

This is because ? is syntax sugar for doing a match on a Try value like this:

    match value_before_question_mark {
        Ok(ok_payload) => ok_payload
        Err(err_payload) => return Err(err_payload)
    }

When you use ? on the value at the end of a function, it changes "implicitly
return this Try value" to "return this Try value if it's an Err, but if it's
Ok, unwrap its Ok payload and return that instead" - which can only possibly
type-check when returning Try(Try(..., ...), ...), which is so unusual that
using ? here is almost always a mistake in practice.

Usually removing the ? here is what makes the most sense, but if you really
want this behavior, make it clear by using an explicit match instead of the ?
syntax sugar.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,OpenCurly,
KwReturn,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
CloseCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,KwElse,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "nested")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty-apply
						(ty (name "Try"))
						(ty (name "Str"))
						(ty-tag-union
							(tags
								(ty (name "Bad")))))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "nested"))
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
						(e-tag (raw "Bad")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "s")))))))
		(s-type-anno (name "tail")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "tail"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(e-question-suffix
							(e-apply
								(e-ident (raw "nested"))
								(e-ident (raw "s"))))))))
		(s-type-anno (name "returned")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "returned"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(e-if-without-else
							(e-binop (op "==")
								(e-ident (raw "s"))
								(e-string
									(e-string-part (raw "x"))))
							(e-block
								(statements
									(s-return
										(e-question-suffix
											(e-apply
												(e-ident (raw "nested"))
												(e-ident (raw "s"))))))))
						(e-if-then-else
							(e-binop (op "==")
								(e-ident (raw "s"))
								(e-string
									(e-string-part (raw "y"))))
							(e-question-suffix
								(e-apply
									(e-ident (raw "nested"))
									(e-ident (raw "s"))))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "s"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nested"))
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
								(e-tag (name "Bad"))))))
				(if-else
					(e-tag (name "Ok")
						(args
							(e-tag (name "Ok")
								(args
									(e-lookup-local
										(p-assign (ident "s"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-apply (name "Try") (builtin)
						(ty-lookup (name "Str") (builtin))
						(ty-tag-union
							(ty-tag-name (name "Bad"))))
					(ty-tag-union
						(ty-tag-name (name "Bad")))))))
	(d-let
		(p-assign (ident "tail"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 451)
								(e-lookup-local
									(p-assign (ident "nested")))
								(e-lookup-local
									(p-assign (ident "s")))))
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
														(p-assign (ident "#err"))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Bad")))))))
	(d-let
		(p-assign (ident "returned"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-expr
					(e-if
						(if-branches
							(if-branch
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "s"))))
									(rhs
										(e-string
											(e-literal (string "x")))))
								(e-block
									(e-return
										(e-match
											(match
												(cond
													(e-call (constraint-fn-var 526)
														(e-lookup-local
															(p-assign (ident "nested")))
														(e-lookup-local
															(p-assign (ident "s")))))
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
																				(p-assign (ident "#err"))))))))))))))))
						(if-else
							(e-empty_record))))
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "s"))))
								(rhs
									(e-string
										(e-literal (string "y")))))
							(e-match
								(match
									(cond
										(e-call (constraint-fn-var 595)
											(e-lookup-local
												(p-assign (ident "nested")))
											(e-lookup-local
												(p-assign (ident "s")))))
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
																	(p-assign (ident "#err"))))))))))))))
					(if-else
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
						(ty-tag-name (name "Bad"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Try(Str, [Bad]), [Bad])"))
		(patt (type "Str -> Try(Str, [Bad])"))
		(patt (type "Str -> Try(Str, [Bad])")))
	(expressions
		(expr (type "Str -> Try(Try(Str, [Bad]), [Bad])"))
		(expr (type "Str -> Try(Str, [Bad])"))
		(expr (type "Str -> Try(Str, [Bad])"))))
~~~
