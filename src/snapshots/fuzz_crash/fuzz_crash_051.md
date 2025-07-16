# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}{0      0)(0}
~~~
# EXPECTED
MISMATCHED BRACE - :0:0:0:0
MISMATCHED BRACE - :0:0:0:0
MISMATCHED BRACE - :0:0:0:0
INVALID STATEMENT - fuzz_crash_051.md:1:9:1:22
# PROBLEMS
**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**MISMATCHED BRACE**
This brace does not match the corresponding opening brace.

**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_051.md:1:9:1:22:**
```roc
module[}{0      0)(0}
```
        ^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:9-1:10),Int(1:10-1:11),Int(1:17-1:18),CloseCurly(1:18-1:19),NoSpaceOpenRound(1:19-1:20),Int(1:20-1:21),CloseRound(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(file @1.1-1.22
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-apply @1.9-1.22
			(e-block @1.9-1.19
				(statements
					(e-int @1.10-1.11 (raw "0"))
					(e-int @1.17-1.18 (raw "0"))))
			(e-int @1.20-1.21 (raw "0")))))
~~~
# FORMATTED
~~~roc
module []
{
	0
	0
}(0)
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
