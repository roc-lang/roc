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
LowerIdent(2:1-2:2),OpColon(2:2-2:3),LowerIdent(2:3-2:4),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @2.1-2.4
	(type-module @2.1-2.2)
	(statements
		(s-type-anno @2.1-2.4 (name "b")
			(ty-var @2.3-2.4 (raw "r")))))
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
