# META
~~~ini
description=parser formatter stability: newline + malformed number token
type=file
~~~
# SOURCE
~~~roc
e={0#
.0.{} }
~~~
# EXPECTED
UNRECOGNIZED SYNTAX - fuzz_crash_104.md:1:4:2:6
# PROBLEMS

┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  e={0#                                                                     │
 │  .0.{} }                                                                   │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_104.md:1:4 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,Int,
DotInt,Dot,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "e"))
			(e-block
				(statements
					(e-nominal-record
						(mapper (e-tuple-access
								(e-int (raw "0"))
								".0"))
						(backing (e-record))))))))
~~~
# FORMATTED
~~~roc
e = {
	(0).0.{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "e"))
		(e-block
			(e-runtime-error (tag "expr_not_canonicalized")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
