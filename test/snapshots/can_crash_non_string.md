# META
~~~ini
description=Test crash with non-string expression
type=expr
~~~
# SOURCE
~~~roc
{
    crash 42
}
~~~
# EXPECTED
CRASH EXPECTS STRING - can_crash_non_string.md:1:1:3:2
# PROBLEMS
**CRASH EXPECTS STRING**
The `crash` keyword expects a string literal as its argument.
For example: `crash "Something went wrong"`
**can_crash_non_string.md:1:1:3:2:**
```roc
{
    crash 42
}
```


# TOKENS
~~~zig
OpenCurly,
KwCrash,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-crash
			(e-int (raw "42")))))
~~~
# FORMATTED
~~~roc
{
	crash 42
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(e-runtime-error (tag "crash_expects_string")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
