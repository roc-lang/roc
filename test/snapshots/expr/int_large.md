# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
# EXPECTED
INVALID NUMBER - int_large.md:1:1:1:31
# PROBLEMS
── ✗ invalid number ─────────────────────────────────────────── int_large.md:1:1

This number literal does not fit in the inferred type.

999999999999999999999999999999
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The inferred type is:

    Dec

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "erroneous_value_expr"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
