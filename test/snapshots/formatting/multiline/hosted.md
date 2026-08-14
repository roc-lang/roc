# META
~~~ini
description=Multiline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [
	a!,
	b!,
]

a! : Str => Str

b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:2:2:2:4
EXPOSED BUT NOT DEFINED - hosted.md:3:2:3:4
DECLARATION HAS NO VALUE - hosted.md:6:1:6:16
DECLARATION HAS NO VALUE - hosted.md:8:1:8:16
# PROBLEMS
── ✗ exposed but not defined ───────────────────────────────────── hosted.md:2:2

The mod header says that a! is exposed, but it is not defined anywhere in
this mod.

a!,
^^

You can fix this by either defining a! in this mod, or by removing it from
the list of exposed values.

── ✗ exposed but not defined ───────────────────────────────────── hosted.md:3:2

The mod header says that b! is exposed, but it is not defined anywhere in
this mod.

b!,
^^

You can fix this by either defining b! in this mod, or by removing it from
the list of exposed values.

── ● declaration has no value ──────────────────────────────────── hosted.md:6:1

This declaration has a type annotation but no implementation.

a! : Str => Str
^^^^^^^^^^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

── ● declaration has no value ──────────────────────────────────── hosted.md:8:1

This declaration has a type annotation but no implementation.

b! : Str => Str
^^^^^^^^^^^^^^^

Add a value body here, or put hosted functions in a platform type mod so
they are published through the host boundary.

# TOKENS
~~~zig
KwHosted,OpenSquare,
LowerIdent,Comma,
LowerIdent,Comma,
CloseSquare,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(hosted
		(exposes
			(exposed-lower-ident
				(text "a!"))
			(exposed-lower-ident
				(text "b!"))))
	(statements
		(s-type-anno (name "a!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))
		(s-type-anno (name "b!")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a!"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "b!"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => Str"))
		(patt (type "Str => Str")))
	(expressions
		(expr (type "Str => Str"))
		(expr (type "Str => Str"))))
~~~
