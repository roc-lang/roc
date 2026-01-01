# META
~~~ini
description=Platform header missing requires keyword
type=file
~~~
# SOURCE
~~~roc
platform "test-platform"
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_missing_requires.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_requires`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_missing_requires.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwPlatform,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "expected_requires"))
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
