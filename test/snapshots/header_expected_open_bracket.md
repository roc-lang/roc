# META
~~~ini
description=Expected an open bracket for the header
type=file
~~~
# SOURCE
~~~roc
module
~~~
# EXPECTED
PARSE ERROR - header_expected_open_bracket.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

**header_expected_open_bracket.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.7
	(malformed-header @1.1-1.1 (tag "header_expected_open_square"))
	(statements))
~~~
# FORMATTED
~~~roc
MALFORMED INPUT
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
