# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{
 
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_035.md:2:2:2:2
COMPILER DIAGNOSTIC - fuzz_crash_035.md:0:0:0:0
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_035.md:2:2:2:2:**
```roc
 
```
 


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**fuzz_crash_035.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:9-1:10),
EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(file @1.1-1.10
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(e-block @1.9-1.10
			(statements))))
~~~
# FORMATTED
~~~roc
module []
{}

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
