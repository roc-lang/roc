# META
~~~ini
description=Platform header missing opening curly for requires
type=file
~~~
# SOURCE
~~~roc
platform "test-platform" requires
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_missing_curly.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_requires_rigids_open_curly`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_missing_curly.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,KwRequires,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_requires_rigids_open_curly"))
	(statements))
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
