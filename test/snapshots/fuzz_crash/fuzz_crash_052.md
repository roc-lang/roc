# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]import
S
0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_052.md:3:1:3:2
MODULE HEADER DEPRECATED - fuzz_crash_052.md:1:1:1:9
MODULE NOT FOUND - fuzz_crash_052.md:1:9:2:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_052.md:3:1:3:2:**
```roc
0
```
^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**fuzz_crash_052.md:1:1:1:9:**
```roc
module[]import
```
^^^^^^^^


**MODULE NOT FOUND**
The module `S` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_052.md:1:9:2:2:**
```roc
module[]import
S
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),KwImport(1:9-1:15),
UpperIdent(2:1-2:2),
Int(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-import @1.9-2.2 (raw "S"))
		(s-malformed @3.1-3.2 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []
import
	S
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.9-2.2 (module "S")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
