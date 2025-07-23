# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{B
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_036.md:1:11:1:11
COMPILER DIAGNOSTIC - fuzz_crash_036.md:0:0:0:0
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_036.md:1:11:1:11:**
```roc
module[]{B
```
          


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_036.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:9-1:10),UpperIdent(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(file @1.1-1.11
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-block @1.9-1.11
			(statements
				(e-tag @1.10-1.11 (raw "B"))))))
~~~
# FORMATTED
~~~roc
module []
B
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
