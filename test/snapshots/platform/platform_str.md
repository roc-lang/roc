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
    provides { processString: "processString" }

processString : Str -> Str
~~~
# EXPECTED
EXPOSED BUT NOT DEFINED - platform_str.md:7:16:7:46
# PROBLEMS
**EXPOSED BUT NOT DEFINED**
The module header says that `processString` is exposed, but it is not defined anywhere in this module.

**platform_str.md:7:16:7:46:**
```roc
    provides { processString: "processString" }
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You can fix this by either defining `processString` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
KwRequires,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
CloseCurly,
KwExposes,OpenSquare,CloseSquare,
KwPackages,OpenCurly,CloseCurly,
KwProvides,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
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
			(record-field (name "processString")
				(e-string
					(e-string-part (raw "processString"))))))
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
	provides { processString: "processString" }

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
