# META
~~~ini
description=Very small number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e-100
~~~
# EXPECTED
INVALID NUMBER - frac_tiny_scientific.md:1:1:1:9
# PROBLEMS

┌────────────────┐
│ INVALID NUMBER ├─ This number literal does not fit in the inferred type. ───┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  1.0e-100                                                                  │
 │  ‾‾‾‾‾‾‾‾                                                                  │
 └─────────────────────────────────────────────── frac_tiny_scientific.md:1:1 ┘

    The inferred type is:

        Dec

# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "1.0e-100"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num-from-numeral)
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
