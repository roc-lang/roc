# META
~~~ini
description=Test deprecated float suffix dec
type=expr
~~~
# SOURCE
~~~roc
3.14dec
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_frac_suffix_dec.md:1:1:1:8
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_frac_suffix_dec.md:1:1:1:8:**
```roc
3.14dec
```
^^^^^^^

The `dec` suffix is no longer supported. Use `3.14.Dec` instead.

# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "3.14dec"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
