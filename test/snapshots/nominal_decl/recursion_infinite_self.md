# META
~~~ini
description=A directly self-recursive nominal declaration with no tag/record indirection is rejected at the declaration
type=snippet
~~~
# SOURCE
~~~roc
T := (T, U64)

t : T
t = T.((t, 1))
~~~
# EXPECTED
INVALID ASSIGNMENT TO ITSELF - recursion_infinite_self.md:4:9:4:10
INVALID RECURSIVE TYPE - recursion_infinite_self.md:1:1:1:14
# PROBLEMS

┌──────────────────────────────┐
│ INVALID ASSIGNMENT TO ITSELF ├─ The value `t` is assigned to itself, ───────┐
└┬─────────────────────────────┘  which would cause an infinite loop at       │
 │                                runtime.                                    │
 │                                                                            │
 │  t = T.((t, 1))                                                            │
 │          ‾                                                                 │
 └──────────────────────────────────────────── recursion_infinite_self.md:4:9 ┘

    Only functions can reference themselves (for recursion). For non-function
    values, the right-hand side must be fully computable without referring to
    the value being assigned.


┌────────────────────────┐
│ INVALID RECURSIVE TYPE ├─ The nominal type T refers to itself in a way ─────┐
└┬───────────────────────┘  that would make it infinite.                      │
 │                                                                            │
 │  T := (T, U64)                                                             │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                             │
 └──────────────────────────────────────────── recursion_infinite_self.md:1:1 ┘

    Its definition is:

        (T, U64)

    Hint: Recursion in a nominal type is only allowed inside a tag union
    payload or record field — for example `ConsList(a) := [Nil, Cons(a,
    ConsList(a))]`.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,Dot,NoSpaceOpenRound,NoSpaceOpenRound,LowerIdent,Comma,Int,CloseRound,CloseRound,
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
		(s-type-anno (name "t")
			(ty (name "T")))
		(s-decl
			(p-ident (raw "t"))
			(e-nominal-apply
				(mapper (e-tag (raw "T")))
				(e-tuple
					(e-ident (raw "t"))
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "t"))
		(e-nominal (nominal "T")
			(e-tuple
				(elems
					(e-runtime-error (tag "self_referential_definition"))
					(e-num (value "1")))))
		(annotation
			(ty-lookup (name "T") (local))))
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
		(patt (type "Error")))
	(type_decls
		(nominal (type "Error")
			(ty-header (name "T"))))
	(expressions
		(expr (type "Error"))))
~~~
