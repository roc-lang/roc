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
CONSTRAINT IN WRONG ANNOTATION - where_clause_enclosing_rigid_error.md:3:29:3:46
# PROBLEMS

┌────────────────────────────────┐
│ CONSTRAINT IN WRONG ANNOTATION ├─ The type variable `a` was introduced by ──┐
└┬───────────────────────────────┘  a different annotation, so this where     │
 │                                  clause cannot add the `show` method to    │
 │                                  it.                                       │
 │                                                                            │
 │  inner : a -> Str where [a.show : a -> Str]                                │
 │                          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                 │
 └──────────────────────────────── where_clause_enclosing_rigid_error.md:3:29 ┘

    A where clause can only add methods to type variables introduced by the
    same annotation. Add this method to the annotation that introduced `a`, or
    use a new type variable here.

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
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-runtime-error (tag "erroneous_value_expr")))
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
