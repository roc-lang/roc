# META
~~~ini
description=parser crash: formatting not stable in formatter round-trip
type=file
~~~
# SOURCE
~~~roc

A:a	where[a.a:(X)->r,a.a:r]B:b	where[b.b:r]C:e->[]h={{()}}
~~~
# EXPECTED
UNDECLARED TYPE VARIABLE - fuzz_crash_105.md:2:3:2:4
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - fuzz_crash_105.md:2:1:2:28
UNDECLARED TYPE VARIABLE - fuzz_crash_105.md:2:30:2:31
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - fuzz_crash_105.md:2:28:2:44
UNDECLARED TYPE VARIABLE - fuzz_crash_105.md:2:46:2:47
EMPTY TUPLE NOT ALLOWED - fuzz_crash_105.md:2:55:2:57
# PROBLEMS

┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `a` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  A:a where[a.a:(X)->r,a.a:r]B:b where[b.b:r]C:e->[]h={{()}}                │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_105.md:2:3 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────────────────────────┐
│ WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION ├─ You cannot define a ────────┐
└┬─────────────────────────────────────────────┘  `where` clause inside a     │
 │                                                type declaration.           │
 │                                                                            │
 │  A:a where[a.a:(X)->r,a.a:r]B:b where[b.b:r]C:e->[]h={{()}}                │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                               │
 └───────────────────────────────────────────────────── fuzz_crash_105.md:2:1 ┘

    You're attempting do this here:


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `b` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  A:a where[a.a:(X)->r,a.a:r]B:b where[b.b:r]C:e->[]h={{()}}                │
 │                               ‾                                            │
 └──────────────────────────────────────────────────── fuzz_crash_105.md:2:30 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────────────────────────┐
│ WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION ├─ You cannot define a ────────┐
└┬─────────────────────────────────────────────┘  `where` clause inside a     │
 │                                                type declaration.           │
 │                                                                            │
 │  A:a where[a.a:(X)->r,a.a:r]B:b where[b.b:r]C:e->[]h={{()}}                │
 │                             ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                               │
 └──────────────────────────────────────────────────── fuzz_crash_105.md:2:28 ┘

    You're attempting do this here:


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `e` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  A:a where[a.a:(X)->r,a.a:r]B:b where[b.b:r]C:e->[]h={{()}}                │
 │                                               ‾                            │
 └──────────────────────────────────────────────────── fuzz_crash_105.md:2:46 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  A:a where[a.a:(X)->r,a.a:r]B:b where[b.b:r]C:e->[]h={{()}}                │
 │                                                        ‾‾                  │
 └──────────────────────────────────────────────────── fuzz_crash_105.md:2:55 ┘

    If you want to represent nothing, try using an empty record: `{}`.

# TOKENS
~~~zig
UpperIdent,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,CloseSquare,UpperIdent,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,CloseSquare,UpperIdent,OpColon,LowerIdent,OpArrow,OpenSquare,CloseSquare,LowerIdent,OpAssign,OpenCurly,OpenCurly,NoSpaceOpenRound,CloseRound,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "A")
				(args))
			(ty-var (raw "a")))
		(s-type-decl
			(header (name "B")
				(args))
			(ty-var (raw "b")))
		(s-type-decl
			(header (name "C")
				(args))
			(ty-fn
				(ty-var (raw "e"))
				(ty-tag-union
					(tags))))
		(s-decl
			(p-ident (raw "h"))
			(e-block
				(statements
					(e-block
						(statements
							(e-tuple))))))))
~~~
# FORMATTED
~~~roc

A : a where [a.a : (X
) -> r, a.a : r]

B : b where [b.b : r]

C : e -> []

h = {
	{
		()
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "h"))
		(e-block
			(e-block
				(e-runtime-error (tag "empty_tuple")))))
	(s-alias-decl
		(ty-header (name "A"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "B"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "C"))
		(ty-fn (effectful false)
			(ty-malformed)
			(ty-tag-union))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(alias (type "A")
			(ty-header (name "A")))
		(alias (type "B")
			(ty-header (name "B")))
		(alias (type "C")
			(ty-header (name "C"))))
	(expressions
		(expr (type "Error"))))
~~~
