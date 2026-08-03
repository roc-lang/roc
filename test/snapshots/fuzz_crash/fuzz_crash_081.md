# META
~~~ini
description=fuzz crash: 0.() incorrectly tokenized as float
type=snippet
~~~
# SOURCE
~~~roc
x = 0.()
~~~
# EXPECTED
EXPECTED RECORD ACCESSOR - fuzz_crash_081.md:1:6:1:7
UNRECOGNIZED SYNTAX - fuzz_crash_081.md:1:5:1:9
# PROBLEMS

┌──────────────────────────┐
│ EXPECTED RECORD ACCESSOR ├─ I was parsing access after `.`, and I ──────────┐
└┬─────────────────────────┘  expected a field name or tuple index.           │
 │                                                                            │
 │  x = 0.()                                                                  │
 │       ‾                                                                    │
 └───────────────────────────────────────────────────── fuzz_crash_081.md:1:6 ┘

    Record access uses a lowercase field name like `.name`. Tuple access uses a
    number like `.0`. Uppercase names, malformed names, and a bare `.` are not
    valid accessors.

    For example:
        person.name
        pair.0

    I found `.` here.


┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  x = 0.()                                                                  │
 │      ‾‾‾‾                                                                  │
 └───────────────────────────────────────────────────── fuzz_crash_081.md:1:5 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,Int,Dot,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-apply
				(e-malformed (reason "expr_dot_suffix_not_allowed"))))))
~~~
# FORMATTED
~~~roc
x = ()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
