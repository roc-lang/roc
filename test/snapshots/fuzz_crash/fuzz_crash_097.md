# META
~~~ini
description=Parser formatting instability (multiline tuple vs lambda)
type=file
~~~
# SOURCE
~~~roc
a=(0(0->X)
->X .a)
~~~
# EXPECTED
MISSING METHOD - fuzz_crash_097.md:1:4:1:5
TYPE MISMATCH - fuzz_crash_097.md:1:4:2:4
# PROBLEMS
── ✗ missing method ────────────────────────────────────── fuzz_crash_097.md:1:4

This from_numeral method is being called on a value whose type doesn't have
that method.

a=(0(0->X)
   ^

The value's type, which does not have a method named from_numeral, is:

    [X(b), ..] -> _ret
      where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]

── ✗ type mismatch ─────────────────────────────────────── fuzz_crash_097.md:1:4

This is not a record, so it does not have any fields to access.

a=(0(0->X)
->X .a)

It is:

    [X(_b), ..]

But I need a record with a a field.

# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,NoSpaceOpenRound,Int,OpArrow,UpperIdent,CloseRound,
OpArrow,UpperIdent,DotLowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-tuple
				(e-field-access
					(receiver
						(e-arrow-call
							(e-apply
								(e-int (raw "0"))
								(e-arrow-call
									(e-int (raw "0"))
									(e-tag (raw "X"))))
							(e-tag (raw "X"))))
					(segment (mode "required") (field "a")))))))
~~~
# FORMATTED
~~~roc
a = (
	0(0 |> X)
		|> X
		.a
)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
