# META
~~~ini
description=parser formatter output instability (stable idempotence)
type=file
~~~
# SOURCE
~~~roc
a=(0->b .c())
~~~
# EXPECTED
NAME NOT IN SCOPE - fuzz_crash_087.md:1:7:1:8
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `b` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  a=(0->b .c())                                                             │
 │        ‾                                                                   │
 └───────────────────────────────────────────────────── fuzz_crash_087.md:1:7 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,OpArrow,LowerIdent,DotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
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
				(e-method-call (method ".c")
					(receiver
						(e-arrow-call
							(e-int (raw "0"))
							(e-ident (raw "b"))))
					(args))))))
~~~
# FORMATTED
~~~roc
a = ((0->b()).c())
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-method-call (method "c")
			(receiver
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-num (value "0"))))
			(args))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
