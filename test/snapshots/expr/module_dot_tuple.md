# META
~~~ini
description=Module dot malformed (should error)
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# EXPECTED
TYPE MISMATCH - module_dot_tuple.md:1:1:1:4
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**module_dot_tuple.md:1:1:1:4:**
```roc
I.5
```
^^^

It has the type:

    (_field, _field2, _field3, _field4, _field5, _field6)

But you are trying to use it as:

    [I, ..]

# TOKENS
~~~zig
UpperIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-tag (raw "I"))
	".5")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "5")
	(e-tag (name "I")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
