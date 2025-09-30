# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc

~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_064.md:2:1:2:1
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This module is named `fuzz_crash_064`, but no top-level type declaration named `fuzz_crash_064` was found.

Add either:
`fuzz_crash_064 := ...` (nominal type)
or:
`fuzz_crash_064 : ...` (type alias)
**fuzz_crash_064.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @2.1-2.1
	(type-module @2.1-2.1)
	(statements))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
