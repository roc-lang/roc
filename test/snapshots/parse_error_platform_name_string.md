# META
~~~ini
description=Platform header with malformed name
type=header
~~~
# SOURCE
~~~roc
platform "test/
~~~
# EXPECTED
UNCLOSED STRING - parse_error_platform_name_string.md:1:10:1:16
PARSE ERROR - parse_error_platform_name_string.md:2:1:2:1
# PROBLEMS
**UNCLOSED STRING**
This string is missing a closing quote.

**parse_error_platform_name_string.md:1:10:1:16:**
```roc
platform "test/
```
         ^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_requires`
This is an unexpected parsing error. Please check your syntax.

**parse_error_platform_name_string.md:2:1:2:1:**
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
(malformed-header (tag "expected_requires"))
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
