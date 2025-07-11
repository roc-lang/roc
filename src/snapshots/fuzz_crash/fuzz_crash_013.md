# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0{
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_013.md:1:1:1:3
INVALID STATEMENT - fuzz_crash_013.md:1:2:1:3
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }

Here is the problematic code:
**fuzz_crash_013.md:1:1:1:3:**
```roc
0{
```
^^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**fuzz_crash_013.md:1:2:1:3:**
```roc
0{
```
 ^


# TOKENS
~~~zig
Int(1:1-1:2),OpenCurly(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(file @1.1-1.3
	(malformed-header @1.1-1.3 (tag "missing_header"))
	(statements
		(e-block @1.2-1.3
			(statements))))
~~~
# FORMATTED
~~~roc
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
