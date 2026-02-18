# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_001.md:1:1:1:3
PARSE ERROR - fuzz_crash_001.md:1:3:1:4
PARSE ERROR - fuzz_crash_001.md:1:4:1:5
MISSING MAIN! FUNCTION - fuzz_crash_001.md:1:1:1:5
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:1:1:3:**
```roc
mo|%
```
^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:3:1:4:**
```roc
mo|%
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_001.md:1:4:1:5:**
```roc
mo|%
```
   ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_001.md:1:1:1:5:**
```roc
mo|%
```
^^^^


# TOKENS
~~~zig
LowerIdent,OpBar,OpPercent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "statement_unexpected_token"))
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
