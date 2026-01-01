# META
~~~ini
description=Module header without exposes
type=file
~~~
# SOURCE
~~~roc
module
~~~
# EXPECTED
PARSE ERROR - parse_error_missing_exposes.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

**parse_error_missing_exposes.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwModule,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_open_square"))
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
