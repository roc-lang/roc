# META
~~~ini
description=Empty tuple literal
type=expr
~~~
# SOURCE
~~~roc
()
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - tuple_empty_unbound.md:1:1:1:3
# PROBLEMS
**EMPTY TUPLE NOT ALLOWED**
I am part way through parsing this tuple, but it is empty:
**tuple_empty_unbound.md:1:1:1:3:**
```roc
()
```
^^

If you want to represent nothing, try using an empty record: `{}`.

# TOKENS
~~~zig
OpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "empty_tuple"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
