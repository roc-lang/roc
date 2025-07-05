# META
~~~ini
description=Empty tuple literal
type=expr
~~~
# SOURCE
~~~roc
()
~~~
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
OpenRound(1:1-1:2),CloseRound(1:2-1:3),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.3)
~~~
# FORMATTED
~~~roc
()
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "empty_tuple"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Error"))
~~~
