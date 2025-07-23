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
INVALID NUMBER LITERAL - expr_int_invalid.md:1:1:1:42
# PROBLEMS
**INVALID NUMBER LITERAL**

**Invalid Number Literal**
The number literal is invalid or too large to represent:
**expr_int_invalid.md:1:1:1:42:**
```roc
99999999999999999999999999999999999999999
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
Int(1:1-1:42),EndOfFile(1:42-1:42),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.42 (raw "99999999999999999999999999999999999999999"))
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
(expr @1.1-1.1 (type "Error"))
~~~
