# META
~~~ini
description=the str test platform with for-clause syntax
type=file
~~~
# SOURCE
~~~roc
platform ""
    requires {
        processString : Str -> Str
    }
    exposes []
    packages {}
    provides { "roc_processString": processString }

processString : Str -> Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_str.md:7:16:7:50
DECLARATION HAS NO VALUE - platform_str.md:9:1:9:27
# PROBLEMS

┌─────────────────────────┐
│ EXPOSED BUT NOT DEFINED ├─ The module header says that `processString` is ──┐
└┬────────────────────────┘  exposed, but it is not defined anywhere in       │
 │                           this module.                                     │
 │                                                                            │
 │  provides { "roc_processString": processString }                           │
 │             ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                             │
 └────────────────────────────────────────────────────── platform_str.md:7:16 ┘

    You can fix this by either defining `processString` in this module, or by
    removing it from the list of exposed values.


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  processString : Str -> Str                                                │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └─────────────────────────────────────────────────────── platform_str.md:9:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,StringStart,StringPart,StringEnd,OpColon,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(platform (name "")
		(requires
			(requires-entry
				(type-aliases)
				(entrypoint "processString")
				(ty-fn
					(ty (name "Str"))
					(ty (name "Str")))))
		(exposes)
		(packages)
		(provides
			(symbol-map-entry (symbol "roc_processString") (func "processString"))))
	(statements
		(s-type-anno (name "processString")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Str"))))))
~~~
# FORMATTED
~~~roc
platform ""
	requires {
		processString : Str -> Str
	}
	exposes []
	packages {}
	provides { "roc_processString": processString }

processString : Str -> Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processString"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Str")))
	(expressions
		(expr (type "Str -> Str"))))
~~~
