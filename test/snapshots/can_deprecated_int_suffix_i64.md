# META
~~~ini
description=Test deprecated integer suffix i64
type=expr
~~~
# SOURCE
~~~roc
-42i64
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_int_suffix_i64.md:1:1:1:7
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_int_suffix_i64.md:1:1:1:7:**
```roc
-42i64
```
^^^^^^

The `i64` suffix is no longer supported. Use `-42.I64` instead.

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "-42i64"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "-42"))
~~~
# TYPES
~~~clojure
(expr (type "I64"))
~~~
