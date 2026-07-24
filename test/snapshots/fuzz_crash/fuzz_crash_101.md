# META
~~~ini
description=Parser formatting non-stable roundtrip
type=file
~~~
# SOURCE
~~~roc
r:(),(->c),(->d)->(c,)
r=|()|(()())
a={
}
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - fuzz_crash_101.md:2:8:2:10
TYPE MISMATCH - fuzz_crash_101.md:2:3:2:13
# PROBLEMS

┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  r=|()|(()())                                                              │
 │         ‾‾                                                                 │
 └───────────────────────────────────────────────────── fuzz_crash_101.md:2:8 ┘

    If you want to represent nothing, try using an empty record: `{}`.


┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  r=|()|(()())                                                              │
 │    ‾‾‾‾‾‾‾‾‾‾                                                              │
 └───────────────────────────────────────────────────── fuzz_crash_101.md:2:3 ┘

    It has the type:

        () -> Error

    But the annotation says it should be:

        (), (({}) -> Error), (({}) -> d) -> Error

    Hint: This function expects 3 arguments but got 1.

# TOKENS
~~~zig
LowerIdent,OpColon,NoSpaceOpenRound,CloseRound,Comma,NoSpaceOpenRound,OpArrow,LowerIdent,CloseRound,Comma,NoSpaceOpenRound,OpArrow,LowerIdent,CloseRound,OpArrow,NoSpaceOpenRound,LowerIdent,Comma,CloseRound,
LowerIdent,OpAssign,OpBar,NoSpaceOpenRound,CloseRound,OpBar,NoSpaceOpenRound,NoSpaceOpenRound,CloseRound,NoSpaceOpenRound,CloseRound,CloseRound,
LowerIdent,OpAssign,OpenCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "r")
			(ty-fn
				(ty-tuple)
				(ty-fn
					(ty-var (raw "c")))
				(ty-fn
					(ty-var (raw "d")))
				(ty-tuple
					(ty-var (raw "c")))))
		(s-decl
			(p-ident (raw "r"))
			(e-lambda
				(args
					(p-tuple))
				(e-tuple
					(e-apply
						(e-tuple)))))
		(s-decl
			(p-ident (raw "a"))
			(e-record))))
~~~
# FORMATTED
~~~roc
r : (),
(() -> c),
(() -> d) -> (
	c,
)
r = |()| (()())

a = {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-lambda
			(args
				(p-tuple
					(patterns)))
			(e-call
				(e-runtime-error (tag "empty_tuple"))))
		(annotation
			(ty-fn (effectful false)
				(ty-tuple)
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "c"))))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var (name "d"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))
	(d-let
		(p-assign (ident "a"))
		(e-empty_record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(), (({}) -> c), (({}) -> d) -> c"))
		(patt (type "{}")))
	(expressions
		(expr (type "(), (({}) -> c), (({}) -> d) -> c"))
		(expr (type "{}"))))
~~~
