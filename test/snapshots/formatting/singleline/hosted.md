# META
~~~ini
description=Singleline formatting hosted
type=file
~~~
# SOURCE
~~~roc
hosted [a!, b!]

a! : Str => Str

b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - hosted.md:1:9:1:11
EXPOSED BUT NOT DEFINED - hosted.md:1:13:1:15
DECLARATION HAS NO VALUE - hosted.md:3:1:3:16
DECLARATION HAS NO VALUE - hosted.md:5:1:5:16
# PROBLEMS

┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `a!` is exposed, ────┐
└┬────────────────────────┘  but it is not defined anywhere in this module.   │
 │                                                                            │
 │  hosted [a!, b!]                                                           │
 │          ‾‾                                                                │
 └───────────────────────────────────────────────────────────── hosted.md:1:9 ┘

    You can fix this by either defining `a!` in this module, or by removing it
    from the list of exposed values.


┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `b!` is exposed, ────┐
└┬────────────────────────┘  but it is not defined anywhere in this module.   │
 │                                                                            │
 │  hosted [a!, b!]                                                           │
 │              ‾‾                                                            │
 └──────────────────────────────────────────────────────────── hosted.md:1:13 ┘

    You can fix this by either defining `b!` in this module, or by removing it
    from the list of exposed values.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  a! : Str => Str                                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                           │
 └───────────────────────────────────────────────────────────── hosted.md:3:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  b! : Str => Str                                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                           │
 └───────────────────────────────────────────────────────────── hosted.md:5:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
KwHosted,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
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
