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
── ● trailing `?` ─────────────────────────── try_suffix_return_mismatch.md:9:10

It's usually a mistake to use a postfix ? on values being returned implicitly
at the end of a function like this:

parse(t)?
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

── ✗ type mismatch ─────────────────────────── try_suffix_return_mismatch.md:8:6

This ? may return early with a type that doesn't match the function body.

t = parse(s)?
    ^^^^^^^^^

If this Try is an Err, then the ? after it immediately returns an Err whose payload has this type:

    [BadInput]

Returning an Err with that type only works if the function itself returns a Try with a compatible error type, but this function's return type is:

    Str

Hint: The function body ends with a ?:

parse(t)?
        ^
That ? unwraps the Try the body would otherwise return. Removing it may fix
this.

── ✗ type mismatch ────────────────────────── try_suffix_return_mismatch.md:14:6

This ? may return early with a type that doesn't match the function body.

t = parse(s)?
    ^^^^^^^^^

If this Try is an Err, then the ? after it immediately returns an Err whose
payload has this type:

    [BadInput]

Returning an Err with that type only works if the function itself returns a Try
with a compatible error type, but this function's return type is:

    Str

Hint: The error types from all ? operators and the function body must be
compatible, since any of them could be the actual return value.

── ✗ type mismatch ────────────────────────── try_suffix_return_mismatch.md:20:7

This ? may return early with a type that doesn't match the function body.

_x = xs.first()?
     ^^^^^^^^^^^

If this Try is an Err, then the ? after it immediately returns an Err whose
payload has this type:

    [ListWasEmpty, ..]

Returning an Err with that type only works if the function itself returns a Try
with a compatible error type, but this function's return type is:

    U64

Hint: The error types from all ? operators and the function body must be
compatible, since any of them could be the actual return value.

── ✗ type mismatch ────────────────────────── try_suffix_return_mismatch.md:30:8

This ? may return early with a type that doesn't match the function body.

_x = l.first()?
     ^^^^^^^^^^

If this Try is an Err, then the ? after it immediately returns an Err whose
payload has this type:

    [ListWasEmpty, ..]

Returning an Err with that type only works if the function itself returns a Try
with a compatible error type, but this function's return type is:

    {}

Hint: The error types from all ? operators and the function body must be
compatible, since any of them could be the actual return value.

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
