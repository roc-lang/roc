# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}0}.a
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_063.md:1:8:1:9
PARSE ERROR - fuzz_crash_063.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `exposed_item_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_063.md:1:8:1:9:**
```roc
module[}0}.a
```
       ^


**PARSE ERROR**
A parsing error occurred: `header_expected_close_square`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_063.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseCurly(1:8-1:9),Int(1:9-1:10),CloseCurly(1:10-1:11),NoSpaceDotLowerIdent(1:11-1:13),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.13
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
