# META
~~~ini
description=Platform header without string for name
type=header
~~~
# SOURCE
~~~roc
platform 123 requires {}
~~~
# EXPECTED
PARSE ERROR - parse_error_platform_start_bad.md:1:10:1:13
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_platform_name_start`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_start_bad.md:1:10:1:13:**
```roc
platform 123 requires {}
```
         ^^^


# TOKENS
~~~zig
KwPlatform,Int,KwRequires,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(malformed-header (tag "expected_platform_name_start"))
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
