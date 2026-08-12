# META
~~~ini
description=A where clause cannot constrain a rigid type variable introduced by a different annotation
type=snippet
~~~
# SOURCE
~~~roc
outer : a -> Str
outer = |value| {
    inner : a -> Str where [a.show : a -> Str]
    inner = |_ignored| "ok"
    inner(value)
}
~~~
# EXPECTED
UNBOUND WHERE RECEIVER - where_clause_enclosing_rigid_error.md:3:29:3:46
# PROBLEMS

┌────────────────────────┐
│ UNBOUND WHERE RECEIVER ├─ The type variable `a` is not introduced by this ──┐
└┬───────────────────────┘  annotation's type or a connected method           │
 │                          constraint, so this where clause cannot add the   │
 │                          `show` method to it.                              │
 │                                                                            │
 │  inner : a -> Str where [a.show : a -> Str]                                │
 │                          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                 │
 └──────────────────────────────── where_clause_enclosing_rigid_error.md:3:29 ┘

    A where clause receiver must be introduced by the annotation's type, or by
    the method type of a receiver that is already connected to the annotation.
    Connect `a` to the annotation, or remove this constraint.

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "outer")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "outer"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(s-type-anno (name "inner")
							(ty-fn
								(ty-var (raw "a"))
								(ty (name "Str")))
							(where
								(method (mod-of "a") (name "show")
									(args
										(ty-var (raw "a")))
									(ty (name "Str")))))
						(s-decl
							(p-ident (raw "inner"))
							(e-lambda
								(args
									(p-ident (raw "_ignored")))
								(e-string
									(e-string-part (raw "ok")))))
						(e-apply
							(e-ident (raw "inner"))
							(e-ident (raw "value")))))))))
~~~
# FORMATTED
~~~roc
outer : a -> Str
outer = |value| {
	inner : a -> Str where [a.show : a -> Str]
	inner = |_ignored| "ok"
	inner(value)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "outer"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str")))
	(expressions
		(expr (type "a -> Str"))))
~~~
