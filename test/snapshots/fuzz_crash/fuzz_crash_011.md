# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_011.md:1:8:1:9
PARSE ERROR - fuzz_crash_011.md:1:9:1:10
PARSE ERROR - fuzz_crash_011.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `header_expected_open_square`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:8:1:9:**
```roc
module P]F
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**fuzz_crash_011.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
KwModule(1:1-1:7),UpperIdent(1:8-1:9),CloseSquare(1:9-1:10),UpperIdent(1:10-1:11),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.11
	(malformed-header @1.8-1.9 (tag "header_expected_open_square"))
	(statements
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
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
