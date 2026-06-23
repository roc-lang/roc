# META
~~~ini
description=Bare .. suggests ..< or ..=
type=snippet
~~~
# SOURCE
~~~roc
r = 1..5
~~~
# EXPECTED
NOT A RANGE OPERATOR - range_bare_double_dot_error.md:1:8:1:9
UNRECOGNIZED SYNTAX - range_bare_double_dot_error.md:1:8:1:9
# PROBLEMS

┌──────────────────────┐
│ NOT A RANGE OPERATOR ├─ .. is not an operator. For an exclusive range use ──┐
└┬─────────────────────┘  ..<; for an inclusive range use ..=.                │
 │                                                                            │
 │  r = 1..5                                                                  │
 │         ‾                                                                  │
 └──────────────────────────────────────── range_bare_double_dot_error.md:1:8 ┘



┌─────────────────────┐
│ UNRECOGNIZED SYNTAX ├─ I don't recognize this syntax. ──────────────────────┐
└┬────────────────────┘                                                       │
 │                                                                            │
 │  r = 1..5                                                                  │
 │         ‾                                                                  │
 └──────────────────────────────────────── range_bare_double_dot_error.md:1:8 ┘

    This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent,OpAssign,Int,DoubleDot,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "r"))
			(e-malformed (reason "expr_double_dot_is_not_range")))))
~~~
# FORMATTED
~~~roc
r = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
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
