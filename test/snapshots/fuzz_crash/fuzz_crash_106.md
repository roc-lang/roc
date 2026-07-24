# META
~~~ini
description=parser crash: formatter output no longer parses after reformat
type=file
source_escapes=true
~~~
# SOURCE
~~~roc
a=0O0\r.0
~~~
# EXPECTED
UPPERCASE BASE - :0:0:0:0
MISPLACED CARRIAGE RETURN - :0:0:0:0
INVALID TUPLE ACCESS - fuzz_crash_106.md:1:3:1:9
# PROBLEMS

UPPERCASE BASE

Number base prefixes must be lowercase (0x, 0o, 0b).



MISPLACED CARRIAGE RETURN

Carriage return characters (\r) are not allowed in Roc source code.



┌──────────────────────┐
│ INVALID TUPLE ACCESS ├─ This value is not a tuple, so it has no .0 ─────────┐
└┬─────────────────────┘  element.                                            │
 │                                                                            │
 │  a=0O0\r.0                                                                  │
 │    ‾‾‾‾‾‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_106.md:1:3 ┘


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,DotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-tuple-access
				(e-int (raw "0O0"))
				".0"))))
~~~
# FORMATTED
~~~roc
a = (0O0).0
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-tuple-access (index "0")
			(e-num (value "0")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
