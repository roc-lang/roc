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
KwModule,OpenSquare,CloseCurly,NoSpaceOpenRound,MalformedSingleQuoteUnclosed,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(malformed-header (tag "header_expected_close_square"))
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
