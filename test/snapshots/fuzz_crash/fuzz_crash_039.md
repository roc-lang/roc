# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}('
)
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_039.md:1:8:1:9
PARSE ERROR - fuzz_crash_039.md:3:1:3:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_039.md:1:8:1:9:**
```roc
module[}('
```
       ^


**PARSE ERROR**
A parsing error occurred: `header_expected_close_square`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_039.md:3:1:3:1:**
```roc

```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseCurly(1:8-1:9),NoSpaceOpenRound(1:9-1:10),MalformedSingleQuoteUnclosed(1:10-1:11),
CloseRound(2:1-2:2),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.2
	(malformed-header @1.1-1.1 (tag "header_expected_close_square"))
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
