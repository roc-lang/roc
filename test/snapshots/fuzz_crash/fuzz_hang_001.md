# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
# EXPECTED
PARSE ERROR - fuzz_hang_001.md:1:1:1:2
PARSE ERROR - fuzz_hang_001.md:1:3:1:4
MISSING MAIN! FUNCTION - fuzz_hang_001.md:1:1:1:4
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_hang_001.md:1:1:1:2:**
```roc
0 (
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_hang_001.md:1:3:1:4:**
```roc
0 (
```
  ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_hang_001.md:1:1:1:4:**
```roc
0 (
```
^^^


# TOKENS
~~~zig
Int,OpenRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))))
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
