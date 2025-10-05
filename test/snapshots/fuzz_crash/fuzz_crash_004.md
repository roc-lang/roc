# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_004.md:2:1:2:1
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_004.md:1:1:1:2
# PROBLEMS
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

**fuzz_crash_004.md:2:1:2:1:**
```roc

```
^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_004`.roc, but no top-level type declaration named `fuzz_crash_004` was found.

Add either:
`fuzz_crash_004 := ...` (nominal type)
or:
`fuzz_crash_004 : ...` (type alias)
**fuzz_crash_004.md:1:1:1:2:**
```roc
F
```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.2
	(type-module @1.1-1.2)
	(statements
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
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
