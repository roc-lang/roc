# META
~~~ini
description=An empty module with no exposes
type=file
~~~
# SOURCE
~~~roc
module []
~~~
# EXPECTED
MODULE HEADER DEPRECATED - module_empty.md:1:1:1:10
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**module_empty.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.10
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
