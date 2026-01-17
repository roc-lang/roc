# META
~~~ini
description=Test exposed annotation-only function
type=file
~~~
# SOURCE
~~~roc
module [myFunc]

myFunc : U64 -> U64
~~~
# EXPECTED
MODULE HEADER DEPRECATED - can_exposed_anno_only.md:1:1:1:16
EXPOSED BUT NOT DEFINED - can_exposed_anno_only.md:1:9:1:15
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**can_exposed_anno_only.md:1:1:1:16:**
```roc
module [myFunc]
```
^^^^^^^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `myFunc` is exposed, but it is not defined anywhere in this module.

**can_exposed_anno_only.md:1:9:1:15:**
```roc
module [myFunc]
```
        ^^^^^^
You can fix this by either defining `myFunc` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "myFunc"))))
	(statements
		(s-type-anno (name "myFunc")
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myFunc"))
		(e-anno-only)
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> U64")))
	(expressions
		(expr (type "U64 -> U64"))))
~~~
