# META
~~~ini
description=Issue #10055: Unstable formatting with record access on lambda call
type=file
~~~
# SOURCE
~~~roc
a=(0->X .a)
~~~
# EXPECTED
TYPE MISMATCH - fuzz_crash_091.md:1:4:1:8
# PROBLEMS
── ✗ type mismatch ─────────────────────────────────────── fuzz_crash_091.md:1:4

This is not a record, so it does not have any fields to access.

a=(0->X .a)
   ^^^^

It is:

    [X(b), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]

But I need a record with a a field.

# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,OpArrow,UpperIdent,DotLowerIdent,CloseRound,
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
							(e-int (raw "0"))
							(e-tag (raw "X"))))
					(segment (mode "required") (field "a")))))))
~~~
# FORMATTED
~~~roc
a = ((0 |> X).a)
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
