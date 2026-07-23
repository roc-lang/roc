# META
~~~ini
description=Issue #10095: Invalid formatting for arrow call on parenthesized expression
type=file
~~~
# SOURCE
~~~roc
t=0->(0)()
~~~
# EXPECTED
MISSING METHOD - fuzz_crash_095.md:1:7:1:8
# PROBLEMS

┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  t=0->(0)()                                                                │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_095.md:1:7 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        a -> _ret where [a.from_numeral : Numeral -> Try(a,
        [InvalidNumeral(Str)])]

# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "t"))
			(e-arrow-call
				(e-int (raw "0"))
				(e-apply
					(e-int (raw "0")))))))
~~~
# FORMATTED
~~~roc
t = 0->(0)()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-call (constraint-fn-var 195)
			(e-num (value "0"))
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~
