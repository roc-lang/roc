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
    provides { multiplyInts: "multiplyInts" }

multiplyInts : I64, I64 -> I64
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_int.md:7:16:7:44
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `multiplyInts` is exposed, but it is not defined anywhere in this module.

**platform_int.md:7:16:7:44:**
```roc
    provides { multiplyInts: "multiplyInts" }
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can fix this by either defining `multiplyInts` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
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
			(record-field (name "multiplyInts")
				(e-string
					(e-string-part (raw "multiplyInts"))))))
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
	provides { multiplyInts: "multiplyInts" }

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
