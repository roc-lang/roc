# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_008.md:1:1:1:2
PARSE ERROR - fuzz_crash_008.md:1:3:1:4
PARSE ERROR - fuzz_crash_008.md:1:4:1:5
MISSING MAIN! FUNCTION - fuzz_crash_008.md:1:1:1:5
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_008.md:1:1:1:2:**
```roc
||1
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_008.md:1:3:1:4:**
```roc
||1
```
  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_008.md:1:4:1:5:**
```roc
||1
```
   ^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_008.md:1:1:1:5:**
```roc
||1
```
^^^^


# TOKENS
~~~zig
OpBar,OpBar,Int,
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
