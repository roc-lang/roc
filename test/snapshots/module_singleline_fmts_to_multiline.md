# META
~~~ini
description=An empty module with a singleline exposes with trailing comma
type=file
~~~
# SOURCE
~~~roc

~~~
# EXPECTED
MISSING MAIN! FUNCTION - module_singleline_fmts_to_multiline.md:2:1:2:1
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**module_singleline_fmts_to_multiline.md:2:1:2:1:**
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
