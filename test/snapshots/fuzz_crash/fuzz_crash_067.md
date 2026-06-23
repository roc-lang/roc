# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
f = || {
    crash 1
}
~~~
# EXPECTED
CRASH EXPECTS STRING - fuzz_crash_067.md:1:8:3:2
# PROBLEMS

┌──────────────────────┐
│ CRASH EXPECTS STRING ├─ The `crash` keyword expects a string literal as ────┐
└┬─────────────────────┘  its argument.                                       │
 │                                                                            │
 │  f = || {                                                                  │
 │      crash 1                                                               │
 │  }                                                                         │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_067.md:1:8 ┘

    For example: `crash "Something went wrong"`

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
KwCrash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-crash
							(e-int (raw "1")))))))))
~~~
# FORMATTED
~~~roc
f = || {
	crash 1
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args)
			(e-block
				(e-runtime-error (tag "crash_expects_string"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> Error")))
	(expressions
		(expr (type "({}) -> Error"))))
~~~
