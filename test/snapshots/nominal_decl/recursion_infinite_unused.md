# META
~~~ini
description=An invalid recursive nominal declaration is rejected even when nothing uses it
type=snippet
~~~
# SOURCE
~~~roc
T := (T, U64)

main = 0
~~~
# EXPECTED
INVALID RECURSIVE TYPE - recursion_infinite_unused.md:1:1:1:14
# PROBLEMS

┌────────────────────────┐
│ INVALID RECURSIVE TYPE ├─ The nominal type T refers to itself in a way ─────┐
└┬───────────────────────┘  that would make it infinite.                      │
 │                                                                            │
 │  T := (T, U64)                                                             │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                             │
 └────────────────────────────────────────── recursion_infinite_unused.md:1:1 ┘

    Its definition is:

        (T, U64)

    Hint: Recursion in a nominal type is only allowed inside a tag union
    payload or record field — for example `ConsList(a) := [Nil, Cons(a,
    ConsList(a))]`.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "T")
				(args))
			(ty-tuple
				(ty (name "T"))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "main"))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-num (value "0")))
	(s-nominal-decl
		(ty-header (name "T"))
		(ty-tuple
			(ty-lookup (name "T") (local))
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(nominal (type "Error")
			(ty-header (name "T"))))
	(expressions
		(expr (type "Dec"))))
~~~
