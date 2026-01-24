# META
~~~ini
description=Test deprecated integer suffix u8
type=expr
~~~
# SOURCE
~~~roc
42u8
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_int_suffix_u8.md:1:1:1:5
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_int_suffix_u8.md:1:1:1:5:**
```roc
42u8
```
^^^^

The `u8` suffix is no longer supported. Use `42.U8` instead.

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "42u8"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "42"))
~~~
# TYPES
~~~clojure
(expr (type "U8"))
~~~
