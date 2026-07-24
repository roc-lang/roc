# META
~~~ini
description=Parser formatter round-trip failure on carriage return byte
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
a=(0\r.e)
~~~
# EXPECTED
MISPLACED CARRIAGE RETURN - :0:0:0:0
MISSING METHOD - fuzz_crash_098.md:1:4:1:5
# PROBLEMS

MISPLACED CARRIAGE RETURN

Carriage return characters (\r) are not allowed in Roc source code.



┌────────────────┐
│ MISSING METHOD ├─ This `from_numeral` method is being called on a value ────┐
└┬───────────────┘  whose type doesn't have that method.                      │
 │                                                                            │
 │  a=(0\r.e)                                                                  │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_098.md:1:4 ┘

    The value's type, which does not have a method named `from_numeral`, is:

        { e: _field, .. }

# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,DotLowerIdent,CloseRound,
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
					(e-int (raw "0"))
					(e-ident (raw ".e")))))))
~~~
# FORMATTED
~~~roc
a = (
	(0).e,
)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-field-access (field "e")
			(receiver
				(e-num (value "0"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_b")))
	(expressions
		(expr (type "_b"))))
~~~
