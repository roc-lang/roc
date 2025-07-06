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
PARSE ERROR - header_expected_open_bracket.md:1:7:1:7
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**header_expected_open_bracket.md:1:7:1:7:**
```roc
module
```
      


# TOKENS
~~~zig
KwModule(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(file @1.1-1.7
	(malformed-header @1.7-1.7 (tag "header_expected_open_square"))
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
