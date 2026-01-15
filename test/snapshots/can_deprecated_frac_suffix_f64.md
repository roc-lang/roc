# META
~~~ini
description=Test deprecated float suffix f64
type=expr
~~~
# SOURCE
~~~roc
3.14f64
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_frac_suffix_f64.md:1:1:1:8
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_frac_suffix_f64.md:1:1:1:8:**
```roc
3.14f64
```
^^^^^^^

The `f64` suffix is no longer supported. Use `3.14.F64` instead.

# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "3.14f64"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 (value "3.14"))
~~~
# TYPES
~~~clojure
(expr (type "F64"))
~~~
