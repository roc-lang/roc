# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
S
0
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_052.md:2:1:2:2
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

**fuzz_crash_052.md:2:1:2:2:**
```roc
0
```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),
Int(2:1-2:2),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.2
	(type-module @1.1-1.2)
	(statements
		(s-malformed @2.1-2.2 (tag "expected_colon_after_type_annotation"))))
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
