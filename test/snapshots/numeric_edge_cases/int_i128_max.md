# META
~~~ini
description=Maximum value for i128 (170141183460469231731687303715884105727)
type=expr
~~~
# SOURCE
~~~roc
170141183460469231731687303715884105727
~~~
# EXPECTED
INVALID NUMBER - int_i128_max.md:1:1:1:40
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  170141183460469231731687303715884105727                                   │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                   │
 └─────────────────────────────────────────────────────── int_i128_max.md:1:1 ┘

    The inferred type is:

        Dec

# TOKENS
~~~zig
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "170141183460469231731687303715884105727"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num (value "170141183460469231731687303715884105727"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
