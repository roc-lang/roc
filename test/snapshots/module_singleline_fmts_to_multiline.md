# META
~~~ini
description=An empty module with a singleline exposes with trailing comma
type=file
~~~
# SOURCE
~~~roc
module [something, SomeType,]
~~~
# EXPECTED
MODULE HEADER DEPRECATED - module_singleline_fmts_to_multiline.md:1:1:1:30
EXPOSED BUT NOT DEFINED - module_singleline_fmts_to_multiline.md:1:9:1:18
EXPOSED BUT NOT DEFINED - module_singleline_fmts_to_multiline.md:1:20:1:28
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**module_singleline_fmts_to_multiline.md:1:1:1:30:**
```roc
module [something, SomeType,]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `something` is exposed, but it is not defined anywhere in this module.

**module_singleline_fmts_to_multiline.md:1:9:1:18:**
```roc
module [something, SomeType,]
```
        ^^^^^^^^^
You can fix this by either defining `something` in this module, or by removing it from the list of exposed values.

**EXPOSED BUT NOT DEFINED**
The module header says that `SomeType` is exposed, but it is not defined anywhere in this module.

**module_singleline_fmts_to_multiline.md:1:20:1:28:**
```roc
module [something, SomeType,]
```
                   ^^^^^^^^
You can fix this by either defining `SomeType` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:18),Comma(1:18-1:19),UpperIdent(1:20-1:28),Comma(1:28-1:29),CloseSquare(1:29-1:30),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.30
	(module @1.1-1.30
		(exposes @1.8-1.30
			(exposed-lower-ident @1.9-1.18
				(text "something"))
			(exposed-upper-ident @1.20-1.28 (text "SomeType"))))
	(statements))
~~~
# FORMATTED
~~~roc
module [
	something,
	SomeType,
]
~~~
# CANONICALIZE
~~~clojure
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
