# META
~~~ini
description=Regression test for issue 8738: Using ? operator on a non-Try type should give a clear TYPE MISMATCH error
type=snippet
~~~
# SOURCE
~~~roc
ok_or : Try(ok, _err), ok -> ok
ok_or = |try, fallback|
	match try {
		_ => fallback
	}

do_something = || {
	# This should error: ok_or returns [Exit I32] which is not an Err type
	_x = ok_or(Err(""), Exit(5))?
	Ok({})
}

result = do_something()
~~~
# EXPECTED
TYPE MISMATCH - issue8738_question_on_non_try.md:9:7:9:30
# PROBLEMS
**TYPE MISMATCH**
The `?` operator expects a `Try` type (a tag union containing ONLY `Ok` and `Err` tags), but I found:
**issue8738_question_on_non_try.md:9:7:9:30:**
```roc
	_x = ok_or(Err(""), Exit(5))?
```
	     ^^^^^^^^^^^^^^^^^^^^^^^

This expression has type:

    [Exit(a), ..] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]

__Tip:__ Maybe wrap a value using `Ok(value)` or `Err(value)`.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,NamedUnderscore,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
Underscore,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "ok_or")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-var (raw "ok"))
					(underscore-ty-var (raw "_err")))
				(ty-var (raw "ok"))
				(ty-var (raw "ok"))))
		(s-decl
			(p-ident (raw "ok_or"))
			(e-lambda
				(args
					(p-ident (raw "try"))
					(p-ident (raw "fallback")))
				(e-match
					(e-ident (raw "try"))
					(branches
						(branch
							(p-underscore)
							(e-ident (raw "fallback")))))))
		(s-decl
			(p-ident (raw "do_something"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-ident (raw "_x"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "ok_or"))
									(e-apply
										(e-tag (raw "Err"))
										(e-string
											(e-string-part (raw ""))))
									(e-apply
										(e-tag (raw "Exit"))
										(e-int (raw "5"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "do_something"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "ok_or"))
		(e-lambda
			(args
				(p-assign (ident "try"))
				(p-assign (ident "fallback")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "try"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-lookup-local
									(p-assign (ident "fallback")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "_err")))
				(ty-rigid-var-lookup (ty-rigid-var (name "ok")))
				(ty-rigid-var-lookup (ty-rigid-var (name "ok"))))))
	(d-let
		(p-assign (ident "do_something"))
		(e-closure
			(captures
				(capture (ident "ok_or")))
			(e-lambda
				(args)
				(e-block
					(s-let
						(p-assign (ident "_x"))
						(e-match
							(match
								(cond
									(e-call
										(e-lookup-local
											(p-assign (ident "ok_or")))
										(e-tag (name "Err")
											(args
												(e-string
													(e-literal (string "")))))
										(e-tag (name "Exit")
											(args
												(e-num (value "5"))))))
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
					(e-tag (name "Ok")
						(args
							(e-empty_record)))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "do_something"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Try(ok, _err), ok -> ok"))
		(patt (type "({}) -> Try({}, err)"))
		(patt (type "Try({}, err)")))
	(expressions
		(expr (type "Try(ok, _err), ok -> ok"))
		(expr (type "({}) -> Try({}, err)"))
		(expr (type "Try({}, err)"))))
~~~
