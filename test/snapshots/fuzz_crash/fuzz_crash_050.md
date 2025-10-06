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
CloseRound(1:1-1:2),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.2
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.2 (tag "statement_unexpected_token"))))
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
