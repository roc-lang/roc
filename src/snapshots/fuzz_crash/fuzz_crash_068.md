# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]({0})
~~~
# EXPECTED
INVALID STATEMENT - fuzz_crash_068.md:1:9:1:14
# PROBLEMS
**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_068.md:1:9:1:14:**
```roc
module[]({0})
```
        ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),NoSpaceOpenRound(1:9-1:10),OpenCurly(1:10-1:11),Int(1:11-1:12),CloseCurly(1:12-1:13),CloseRound(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(file @1.1-1.14
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-tuple @1.9-1.14
			(e-block @1.10-1.13
				(statements
					(e-int @1.11-1.12 (raw "0")))))))
~~~
# FORMATTED
~~~roc
module []
(
	{
		0
	},
)
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
