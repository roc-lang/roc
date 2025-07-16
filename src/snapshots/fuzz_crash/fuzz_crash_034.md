# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]0 f
~~~
# EXPECTED
INVALID STATEMENT - fuzz_crash_034.md:1:9:1:10
INVALID STATEMENT - fuzz_crash_034.md:1:11:1:12
# PROBLEMS
**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_034.md:1:9:1:10:**
```roc
module[]0 f
```
        ^


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_034.md:1:11:1:12:**
```roc
module[]0 f
```
          ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),Int(1:9-1:10),LowerIdent(1:11-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(file @1.1-1.12
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-int @1.9-1.10 (raw "0"))
		(e-ident @1.11-1.12 (raw "f"))))
~~~
# FORMATTED
~~~roc
module []
0
f
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
