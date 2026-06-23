# META
~~~ini
description=Multiline without comma formatting package
type=file
~~~
# SOURCE
~~~roc
package
	[
		a!,
		b!
	]
	{
		a: "a",
		b: "b"
	}

a! : Str => Str
b! : Str => Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - package.md:3:3:3:5
EXPOSED BUT NOT DEFINED - package.md:4:3:4:5
DECLARATION HAS NO VALUE - package.md:11:1:11:16
DECLARATION HAS NO VALUE - package.md:12:1:12:16
# PROBLEMS

┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `a!` is exposed, ────┐
└┬────────────────────────┘  but it is not defined anywhere in this module.   │
 │                                                                            │
 │  a!,                                                                       │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────────── package.md:3:3 ┘

    You can fix this by either defining `a!` in this module, or by removing it
    from the list of exposed values.


┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `b!` is exposed, ────┐
└┬────────────────────────┘  but it is not defined anywhere in this module.   │
 │                                                                            │
 │  b!                                                                        │
 │  ‾‾                                                                        │
 └──────────────────────────────────────────────────────────── package.md:4:3 ┘

    You can fix this by either defining `b!` in this module, or by removing it
    from the list of exposed values.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  a! : Str => Str                                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                           │
 └─────────────────────────────────────────────────────────── package.md:11:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  b! : Str => Str                                                           │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                           │
 └─────────────────────────────────────────────────────────── package.md:12:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
KwPackage,
OpenSquare,
LowerIdent,Comma,
LowerIdent,
CloseSquare,
OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(package
		(exposes
			(exposed-lower-ident
				(text "a!"))
			(exposed-lower-ident
				(text "b!")))
		(packages
			(record-field (name "a")
				(e-string
					(e-string-part (raw "a"))))
			(record-field (name "b")
				(e-string
					(e-string-part (raw "b"))))))
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
package
	[
		a!,
		b!,
	]
	{
		a: "a",
		b: "b",
	}

a! : Str => Str

b! : Str => Str
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
