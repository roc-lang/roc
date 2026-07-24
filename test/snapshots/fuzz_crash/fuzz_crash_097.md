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

┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  a=(0(0->X)                                                                │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_097.md:1:4 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        [X(b), ..] -> _ret
          where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]


┌───────────────┐
│ TYPE MISMATCH ├─ This is not a record, so it does not have any fields to ───┐
└┬──────────────┘  access.                                                    │
 │                                                                            │
 │  a=(0(0->X)                                                                │
 │  ->X .a)                                                                   │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_097.md:1:4 ┘

    It is:

        [X(_b), ..]

    But I need a record with a `a` field.

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
					(e-arrow-call
						(e-apply
							(e-int (raw "0"))
							(e-arrow-call
								(e-int (raw "0"))
								(e-tag (raw "X"))))
						(e-tag (raw "X")))
					(e-ident (raw ".a")))))))
~~~
# FORMATTED
~~~roc
a = (
	(
		0(0->X)
			->X,
	).a,
)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-field-access (field "a")
			(receiver
				(e-tag (name "X")
					(args
						(e-call (constraint-fn-var 196)
							(e-num (value "0"))
							(e-tag (name "X")
								(args
									(e-num (value "0")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_b")))
	(expressions
		(expr (type "_b"))))
~~~
