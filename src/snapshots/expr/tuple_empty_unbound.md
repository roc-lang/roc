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
COMPILER DIAGNOSTIC - tuple_empty_unbound.md:0:0:0:0
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'empty_tuple' is not yet handled in report generation.
**tuple_empty_unbound.md:0:0:0:0**

# TOKENS
~~~zig
OpenRound(1:1-1:2),CloseRound(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.3)
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
(expr @1.1-1.1 (type "Error"))
~~~
