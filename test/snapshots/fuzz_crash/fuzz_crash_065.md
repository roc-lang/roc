# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]{R}
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_065.md:1:9:1:10
PARSE ERROR - fuzz_crash_065.md:1:11:1:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_065.md:1:9:1:10:**
```roc
module[]{R}
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

**fuzz_crash_065.md:1:11:1:12:**
```roc
module[]{R}
```
          ^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:7-1:8),CloseSquare(1:8-1:9),OpenCurly(1:9-1:10),UpperIdent(1:10-1:11),CloseCurly(1:11-1:12),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.12
	(module @1.1-1.9
		(exposes @1.7-1.9))
	(statements
		(s-malformed @1.9-1.10 (tag "statement_unexpected_token"))
		(s-malformed @1.11-1.12 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module []
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
