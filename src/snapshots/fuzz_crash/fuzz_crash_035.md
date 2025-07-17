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
INVALID STATEMENT - fuzz_crash_035.md:1:9:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**fuzz_crash_035.md:2:2:2:2:**
```roc
 
```
 


**INVALID STATEMENT**
The statement `expression` is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_035.md:1:9:1:10:**
```roc
module[]{
```
        ^


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
