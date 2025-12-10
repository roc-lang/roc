# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_004.md:2:1:2:1
MISSING MAIN! FUNCTION - fuzz_crash_004.md:1:1:1:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_004.md:1:1:1:2:**
```roc
F
```
^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_004.md:1:1:1:2:**
```roc
F
```
^


# TOKENS
~~~zig
UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
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
