# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc

~~~
# EXPECTED
MISSING MAIN! FUNCTION - fuzz_crash_064.md:2:1:2:1
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_064.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
