# META
~~~ini
description=A local binding with a structurally infinite type is caught even when it never escapes into the enclosing def's type
type=snippet
~~~
# SOURCE
~~~roc
f = |_x| {
    bad = |x| bad([x])
    0
}
~~~
# EXPECTED
INFINITE TYPE - local_binding_infinite_type.md:2:5:2:8
# PROBLEMS

┌───────────────┐
│ INFINITE TYPE ├─ I am inferring a weird self-referential type. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  bad = |x| bad([x])                                                        │
 │  ‾‾‾                                                                       │
 └──────────────────────────────────────── local_binding_infinite_type.md:2:5 ┘

    Here is my best effort at writing down the type. You will see
    `<RecursiveType>` for parts of the type that repeat infinitely.

        List(<RecursiveType>)

# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,CloseSquare,CloseRound,
Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "_x")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "bad"))
							(e-lambda
								(args
									(p-ident (raw "x")))
								(e-apply
									(e-ident (raw "bad"))
									(e-list
										(e-ident (raw "x"))))))
						(e-int (raw "0"))))))))
~~~
# FORMATTED
~~~roc
f = |_x| {
	bad = |x| bad([x])
	0
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "_x")))
			(e-block
				(s-let
					(p-assign (ident "bad"))
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-call (constraint-fn-var 187)
							(e-runtime-error (tag "erroneous_value_use"))
							(e-list
								(elems
									(e-lookup-local
										(p-assign (ident "x"))))))))
				(e-num (value "0"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
