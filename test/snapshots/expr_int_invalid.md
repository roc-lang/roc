# META
~~~ini
description=Invalid integer literal that exceeds i128 range
type=expr
~~~
# SOURCE
~~~roc
99999999999999999999999999999999999999999
~~~
# EXPECTED
INVALID NUMBER - expr_int_invalid.md:1:1:1:42
# PROBLEMS
**INVALID NUMBER**
This number literal is not valid: `99999999999999999999999999999999999999999`

**expr_int_invalid.md:1:1:1:42:**
```roc
99999999999999999999999999999999999999999
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Check that the number is correctly formatted. Valid examples include: `42`, `3.14`, `0x1A`, or `1_000_000`.

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "99999999999999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "invalid_num_literal"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
