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
MODULE NOT FOUND - fuzz_crash_052.md:1:9:2:2
INVALID STATEMENT - fuzz_crash_052.md:3:1:3:2
# PROBLEMS
**MODULE NOT FOUND**
The module `s` was not found in this Roc project.

You're attempting to use this module here:
**fuzz_crash_052.md:1:9:2:2:**
```roc
module[]import
S
```


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_052.md:3:1:3:2:**
```roc
0
```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),KwImport(1:9-1:15),
UpperIdent(2:1-2:2),
Int(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(file @1.1-3.2
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-import @1.9-2.2 (raw "S"))
		(e-int @3.1-3.2 (raw "0"))))
~~~
# FORMATTED
~~~roc
module []
import
	S
0
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
