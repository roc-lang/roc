# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
)
 
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_050.md:1:1:1:2
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_050.md:1:1:1:2:**
```roc
)
```
^


# TOKENS
~~~zig
CloseRound,
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
