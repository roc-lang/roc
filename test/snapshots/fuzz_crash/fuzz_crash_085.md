# META
~~~ini
description=fuzz regression: canonicalize type annotation arg resolves to non-rigid
type=file
~~~
# SOURCE
~~~roc
C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - fuzz_crash_085.md:1:3:1:4
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:14:1:15
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:16:1:17
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:23:1:24
UNDECLARED TYPE VARIABLE - fuzz_crash_085.md:1:27:1:28
EMPTY TUPLE NOT ALLOWED - fuzz_crash_085.md:1:32:1:34
# PROBLEMS

┌──────────────────────────┐
│ UNDERSCORE IN TYPE ALIAS ├─ Underscores are not allowed in type alias ──────┐
└┬─────────────────────────┘  declarations.                                   │
 │                                                                            │
 │  C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}                                      │
 │    ‾                                                                       │
 └───────────────────────────────────────────────────── fuzz_crash_085.md:1:3 ┘

    Underscores in type annotations mean "I don't care about this type", which
    doesn't make sense when declaring a type. If you need a placeholder type
    variable, use a named type variable like `a` instead.


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `a` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}                                      │
 │               ‾                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_085.md:1:14 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `b` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}                                      │
 │                 ‾                                                          │
 └──────────────────────────────────────────────────── fuzz_crash_085.md:1:16 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `r` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}                                      │
 │                        ‾                                                   │
 └──────────────────────────────────────────────────── fuzz_crash_085.md:1:23 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `e` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}                                      │
 │                            ‾                                               │
 └──────────────────────────────────────────────────── fuzz_crash_085.md:1:27 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  C(_,b):()D:C(a,b)E:{b:r}F:e r={(){}}                                      │
 │                                 ‾‾                                         │
 └──────────────────────────────────────────────────── fuzz_crash_085.md:1:32 ┘

    If you want to represent nothing, try using an empty record: `{}`.

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpColon,NoSpaceOpenRound,CloseRound,UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,UpperIdent,OpColon,LowerIdent,LowerIdent,OpAssign,OpenCurly,NoSpaceOpenRound,CloseRound,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "C")
				(args
					(_)
					(ty-var (raw "b"))))
			(ty-tuple))
		(s-type-decl
			(header (name "D")
				(args))
			(ty-apply
				(ty (name "C"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "E")
				(args))
			(ty-record
				(anno-record-field (name "b")
					(ty-var (raw "r")))))
		(s-type-decl
			(header (name "F")
				(args))
			(ty-var (raw "e")))
		(s-decl
			(p-ident (raw "r"))
			(e-block
				(statements
					(e-tuple)
					(e-record))))))
~~~
# FORMATTED
~~~roc
C(_, b) : ()

D : C(a, b)

E : { b : r }

F : e

r = {
	()
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-block
			(s-expr
				(e-runtime-error (tag "empty_tuple")))
			(e-empty_record)))
	(s-alias-decl
		(ty-header (name "C")
			(ty-args
				(ty-underscore)
				(ty-rigid-var (name "b"))))
		(ty-tuple))
	(s-alias-decl
		(ty-header (name "D"))
		(ty-apply (name "C") (local)
			(ty-malformed)
			(ty-malformed)))
	(s-alias-decl
		(ty-header (name "E"))
		(ty-record
			(field (field "b")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "F"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(type_decls
		(alias (type "C(Error, b)")
			(ty-header (name "C")
				(ty-args
					(ty-underscore)
					(ty-rigid-var (name "b")))))
		(alias (type "D")
			(ty-header (name "D")))
		(alias (type "E")
			(ty-header (name "E")))
		(alias (type "F")
			(ty-header (name "F"))))
	(expressions
		(expr (type "{}"))))
~~~
