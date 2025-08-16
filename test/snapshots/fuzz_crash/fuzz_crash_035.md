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
PARSE ERROR - fuzz_crash_035.md:1:9:1:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

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
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []


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
