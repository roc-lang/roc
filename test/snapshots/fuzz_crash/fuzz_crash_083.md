# META
~~~ini
description=fuzz regression: type_parameter_conflict diagnostic report
type=file
~~~
# SOURCE
~~~roc
A(a) : a where [a.a1 : (a, a) -> Str]
C(b, b) : (a, b)
D(a, b) : C(a, b)
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - fuzz_crash_083.md:1:1:1:38
TYPE PARAMETER CONFLICT - fuzz_crash_083.md:2:6:2:7
UNDECLARED TYPE VARIABLE - fuzz_crash_083.md:2:12:2:13
TOO MANY ARGS - fuzz_crash_083.md:3:11:3:18
# PROBLEMS

┌──────────────────────────────────────────────┐
│ WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION ├─ You cannot define a ────────┐
└┬─────────────────────────────────────────────┘  `where` clause inside a     │
 │                                                type declaration.           │
 │                                                                            │
 │  A(a) : a where [a.a1 : (a, a) -> Str]                                     │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                     │
 └───────────────────────────────────────────────────── fuzz_crash_083.md:1:1 ┘

    You're attempting do this here:


┌─────────────────────────┐
│ TYPE PARAMETER CONFLICT ├─ The type parameter `b` in type `C` conflicts ────┐
└┬────────────────────────┘  with another declaration.                        │
 │                                                                            │
 │  C(b, b) : (a, b)                                                          │
 │       ‾                                                                    │
 └───────────────────────────────────────────────────── fuzz_crash_083.md:2:6 ┘

    Type parameters must have unique names within their scope.

    The conflicting parameter is here:

    But `b` was already declared here:
      ┌───────────────────────────────────────────────────────────────────────┐
    2 │  C(b, b) : (a, b)                                                     │
      │    ‾                                                                  │
      └──────────────────────────────────────────────── fuzz_crash_083.md:2:3 ┘


┌──────────────────────────┐
│ UNDECLARED TYPE VARIABLE ├─ The type variable `a` is not declared in this ──┐
└┬─────────────────────────┘  scope.                                          │
 │                                                                            │
 │  C(b, b) : (a, b)                                                          │
 │             ‾                                                              │
 └──────────────────────────────────────────────────── fuzz_crash_083.md:2:12 ┘

    Type variables must be introduced in a type annotation before they can be
    used.


┌───────────────┐
│ TOO MANY ARGS ├─ The type C expects 1 argument, but got 2 instead. ─────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  D(a, b) : C(a, b)                                                         │
 │            ‾‾‾‾‾‾‾                                                         │
 └──────────────────────────────────────────────────── fuzz_crash_083.md:3:11 ┘


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "A")
				(args
					(ty-var (raw "a"))))
			(ty-var (raw "a")))
		(s-type-decl
			(header (name "C")
				(args
					(ty-var (raw "b"))
					(ty-var (raw "b"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))))
		(s-type-decl
			(header (name "D")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-apply
				(ty (name "C"))
				(ty-var (raw "a"))
				(ty-var (raw "b"))))))
~~~
# FORMATTED
~~~roc
A(a) : a where [a.a1 : (a, a) -> Str]

C(b, b) : (a, b)

D(a, b) : C(a, b)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "A")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
	(s-alias-decl
		(ty-header (name "C")
			(ty-args
				(ty-rigid-var (name "b"))))
		(ty-tuple
			(ty-malformed)
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
	(s-alias-decl
		(ty-header (name "D")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-apply (name "C") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "A(a)")
			(ty-header (name "A")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "C(b)")
			(ty-header (name "C")
				(ty-args
					(ty-rigid-var (name "b")))))
		(alias (type "D(a, b)")
			(ty-header (name "D")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions))
~~~
