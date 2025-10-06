# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
import#\
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_078.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_078.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwImport(1:1-1:7),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.7
	(type-module @1.1-1.7)
	(statements
		(s-malformed @1.1-1.7 (tag "incomplete_import"))))
~~~
# FORMATTED
~~~roc
# \
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
