# META
~~~ini
description=When is old syntax use match instead (should error)
type=expr
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 3 -> 4
~~~
# EXPECTED
UNDEFINED VARIABLE - when_with_numbers.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**

**Undefined Variable**
The variable 'when' is not defined:
**when_with_numbers.md:1:1:1:5:**
```roc
when x is
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),
Int(2:2-2:3),OpArrow(2:4-2:6),Int(2:7-2:8),
Int(3:2-3:3),OpArrow(3:4-3:6),Int(3:7-3:8),EndOfFile(3:8-3:8),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
