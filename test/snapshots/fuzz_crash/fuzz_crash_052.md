# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
S
0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_052.md:2:1:2:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_052.md:1:1:1:2:**
```roc
S
```
^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_052.md:2:1:2:2:**
```roc
0
```
^


# TOKENS
~~~zig
UpperIdent,
Int,
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
