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
KwImport,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "incomplete_import"))))
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
