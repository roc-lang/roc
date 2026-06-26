# META
~~~ini
description=the int test platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {
        multiplyInts : I64, I64 -> I64
    }
    exposes []
    packages {}
    provides { "roc_multiplyInts": multiplyInts }

multiplyInts : I64, I64 -> I64
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_int.md:7:16:7:48
DECLARATION HAS NO VALUE - platform_int.md:9:1:9:31
# PROBLEMS

┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `multiplyInts` is ───┐
└┬────────────────────────┘  exposed, but it is not defined anywhere in       │
 │                           this module.                                     │
 │                                                                            │
 │  provides { "roc_multiplyInts": multiplyInts }                             │
 │             ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                               │
 └────────────────────────────────────────────────────── platform_int.md:7:16 ┘

    You can fix this by either defining `multiplyInts` in this module, or by
    removing it from the list of exposed values.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  multiplyInts : I64, I64 -> I64                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
 └─────────────────────────────────────────────────────── platform_int.md:9:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires
			(requires-entry
				(type-aliases)
				(entrypoint "multiplyInts")
				(ty-fn
					(ty (name "I64"))
					(ty (name "I64"))
					(ty (name "I64")))))
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_multiplyInts") (func "multiplyInts"))))
	(statements
		(s-type-anno (name "multiplyInts")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))
				(ty (name "I64"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {
		multiplyInts : I64, I64 -> I64
	}
	exposes []
	packages {}
	provides { "roc_multiplyInts": multiplyInts }

multiplyInts : I64, I64 -> I64
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "multiplyInts"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64, I64 -> I64")))
	(expressions
		(expr (type "I64, I64 -> I64"))))
~~~
