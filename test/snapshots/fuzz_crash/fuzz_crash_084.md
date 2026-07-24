# META
~~~ini
description=fuzz regression: parser formatter output instability
type=snippet
source_escapes=true
~~~
# SOURCE
~~~roc
t=|(0|(#\r))|0
~~~
# EXPECTED
MISPLACED CARRIAGE RETURN - :0:0:0:0
NOT IMPLEMENTED - fuzz_crash_084.md:1:5:1:11
# PROBLEMS

MISPLACED CARRIAGE RETURN

Carriage return characters (\r) are not allowed in Roc source code.



┌─────────────────┐
│ NOT IMPLEMENTED ├─ This feature is not yet implemented: alternatives ───────┐
└┬────────────────┘  pattern outside match expression.                        │
 │                                                                            │
 │  t=|(0|(#\r))|0                                                             │
 │      ‾‾‾‾‾‾                                                                │
 └───────────────────────────────────────────────────── fuzz_crash_084.md:1:5 ┘

    This error doesn't have a proper diagnostic report yet. Let us know if you
    want to help improve Roc's error messages!

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,Int,OpBar,NoSpaceOpenRound,CloseRound,CloseRound,OpBar,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "t"))
			(e-lambda
				(args
					(p-tuple
						(p-alternatives
							(p-int (raw "0"))
							(p-tuple))))
				(e-int (raw "0"))))))
~~~
# FORMATTED
~~~roc
t = |
	(
		0
		| ( #
		),
	),
| 0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-lambda
			(args
				(p-tuple
					(patterns
						(p-runtime-error (tag "not_implemented")))))
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error) -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "(Error) -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
