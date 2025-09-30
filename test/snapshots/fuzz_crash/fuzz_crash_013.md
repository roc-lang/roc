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
PARSE ERROR - fuzz_crash_013.md:1:1:1:2
PARSE ERROR - fuzz_crash_013.md:1:2:1:3
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_013.md:1:1:1:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_013.md:1:1:1:2:**
```roc
0{
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_013.md:1:2:1:3:**
```roc
0{
```
 ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This module is named `fuzz_crash_013`, but no top-level type declaration named `fuzz_crash_013` was found.

Add either:
`fuzz_crash_013 := ...` (nominal type)
or:
`fuzz_crash_013 : ...` (type alias)
**fuzz_crash_013.md:1:1:1:3:**
```roc
0{
```
^^


# TOKENS
~~~zig
Int(1:1-1:2),OpenCurly(1:2-1:3),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.3
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.2 (tag "statement_unexpected_token"))
		(s-malformed @1.2-1.3 (tag "statement_unexpected_token"))))
~~~
# FORMATTED
~~~roc
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
