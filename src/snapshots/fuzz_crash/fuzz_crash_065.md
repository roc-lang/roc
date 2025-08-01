# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{R}
~~~
# EXPECTED
INVALID STATEMENT - fuzz_crash_065.md:1:9:1:12
# PROBLEMS
**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_065.md:1:9:1:12:**
```roc
module[]{R}
```
        ^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:9-1:10),UpperIdent(1:10-1:11),CloseCurly(1:11-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(file @1.1-1.12
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-block @1.9-1.12
			(statements
				(e-tag @1.10-1.11 (raw "R"))))))
~~~
# FORMATTED
~~~roc
module []
{
	R
}
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
