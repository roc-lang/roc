# META
~~~ini
description=Issue #10047: Unstable formatting for parser tuple vs lambda grouping
type=file
~~~
# SOURCE
~~~roc
d=(0||())
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - fuzz_crash_093.md:1:7:1:9
# PROBLEMS

┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  d=(0||())                                                                 │
 │        ‾‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_093.md:1:7 ┘

    If you want to represent nothing, try using an empty record: `{}`.

# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,OpBar,OpBar,NoSpaceOpenRound,CloseRound,CloseRound,
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
				(e-int (raw "0"))
				(e-lambda
					(args)
					(e-tuple))))))
~~~
# FORMATTED
~~~roc
d = (0, || ())
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "d"))
		(e-tuple
			(elems
				(e-num (value "0"))
				(e-lambda
					(args)
					(e-runtime-error (tag "empty_tuple")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Dec, ({}) -> Error)")))
	(expressions
		(expr (type "(Dec, ({}) -> Error)"))))
~~~
