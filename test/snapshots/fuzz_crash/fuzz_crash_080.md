# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
c : L
        where [
                o
                .h : a,
        ]
~~~
# EXPECTED
UNDECLARED TYPE - fuzz_crash_080.md:1:5:1:6
UNBOUND WHERE RECEIVER - fuzz_crash_080.md:3:17:4:23
DECLARATION HAS NO VALUE - fuzz_crash_080.md:1:1:5:10
# PROBLEMS

┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `L` is not declared in this scope. ─────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  c : L                                                                     │
 │      ‾                                                                     │
 └───────────────────────────────────────────────────── fuzz_crash_080.md:1:5 ┘



┌────────────────────────┐
│ UNBOUND WHERE RECEIVER ├─ The type variable `o` is not introduced by this ──┐
└┬───────────────────────┘  annotation's type or a connected method           │
 │                          constraint, so this where clause cannot add the   │
 │                          `h` method to it.                                 │
 │                                                                            │
 │  o                                                                         │
 │  .h : a,                                                                   │
 │                                                                            │
 └──────────────────────────────────────────────────── fuzz_crash_080.md:3:17 ┘

    A where clause receiver must be introduced by the annotation's type, or by
    the method type of a receiver that is already connected to the annotation.
    Connect `o` to the annotation, or remove this constraint.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  c : L                                                                     │
 │          where [                                                           │
 │                  o                                                         │
 │                  .h : a,                                                   │
 │          ]                                                                 │
 │                                                                            │
 └───────────────────────────────────────────────────── fuzz_crash_080.md:1:1 ┘

    Add a value body here, or put hosted functions in a platform type mod so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
KwWhere,OpenSquare,
LowerIdent,
DotLowerIdent,OpColon,LowerIdent,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "c")
			(ty (name "L"))
			(where
				(method (mod-of "o") (name "h")
					(args)
					(ty-var (raw "a")))))))
~~~
# FORMATTED
~~~roc
c : L
	where [
		o
		.h : a,
	]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "c"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-malformed)
			(where
				(method (ty-rigid-var (name "o")) (name "h")
					(args)
					(ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
