# META
~~~ini
description=formatter instability with leading newline
type=file
~~~
# SOURCE
~~~roc

b:r
~~~
# EXPECTED
MISSING MAIN! FUNCTION - fuzz_crash_079.md:2:1:2:4
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_079.md:2:1:2:4:**
```roc
b:r
```
^^^


# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "b")
			(ty-var (raw "r")))))
~~~
# FORMATTED
~~~roc

b : r
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
