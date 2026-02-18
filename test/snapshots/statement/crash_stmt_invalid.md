# META
~~~ini
description=Crash statement with invalid non-string argument
type=statement
~~~
# SOURCE
~~~roc
crash 42
~~~
# EXPECTED
CRASH EXPECTS STRING - crash_stmt_invalid.md:1:1:1:9
# PROBLEMS
**CRASH EXPECTS STRING**
The `crash` keyword expects a string literal as its argument.
For example: `crash "Something went wrong"`
**crash_stmt_invalid.md:1:1:1:9:**
```roc
crash 42
```
^^^^^^^^


# TOKENS
~~~zig
KwCrash,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-crash
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-runtime-error (tag "crash_expects_string")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
