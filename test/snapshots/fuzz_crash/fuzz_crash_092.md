# META
~~~ini
description=Issue #10056: Unstable formatting with escaped record literal header
type=file
~~~
# SOURCE
~~~roc
d=(0->X .a)
~~~
# EXPECTED
TYPE MISMATCH - fuzz_crash_092.md:1:4:1:8
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This is not a record, so it does not have any fields to ───┐
└┬──────────────┘  access.                                                    │
 │                                                                            │
 │  d=(0->X .a)                                                               │
 │     ‾‾‾‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_092.md:1:4 ┘

    It is:

        [X(b), ..] where [b.from_numeral : Numeral -> Try(b,
        [InvalidNumeral(Str)])]

    But I need a record with a `a` field.

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
			(p-ident (raw "d"))
			(e-tuple
				(e-field-access
					(e-arrow-call
						(e-int (raw "0"))
						(e-tag (raw "X")))
					(e-ident (raw ".a")))))))
~~~
# FORMATTED
~~~roc
d = ((0->X).a)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "d"))
		(e-field-access (field "a")
			(receiver
				(e-tag (name "X")
					(args
						(e-num (value "0"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_b")))
	(expressions
		(expr (type "_b"))))
~~~
